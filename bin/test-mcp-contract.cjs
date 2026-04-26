#!/usr/bin/env node
/* eslint-env node */
/* global fetch, process, setTimeout */

const { spawn } = require("node:child_process");
const Ajv = require("ajv");

const BASE_URL = (process.env.MB_MCP_BASE_URL || "http://localhost:3000").replace(/\/$/, "");
const START_COMMAND =
  process.env.MB_MCP_START_COMMAND || "clojure -M:run:dev:dev-start --hot";
const START_TIMEOUT_MS = Number(process.env.MB_MCP_START_TIMEOUT_MS || 300000);
const HEALTH_POLL_MS = Number(process.env.MB_MCP_HEALTH_POLL_MS || 2000);
const WAREHOUSE_CARD_NAME = process.env.MB_MCP_WAREHOUSE_CARD_NAME || "Warehouses";
const WAREHOUSE_STATE = process.env.MB_MCP_WAREHOUSE_STATE || "NC";

let startedServer = null;

function log(message) {
  process.stdout.write(`${message}\n`);
}

function fail(message) {
  throw new Error(message);
}

function assert(condition, message) {
  if (!condition) {
    fail(message);
  }
}

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function fetchJson(path, options = {}) {
  const response = await fetch(`${BASE_URL}${path}`, {
    ...options,
    headers: {
      Accept: "application/json",
      ...(options.body ? { "Content-Type": "application/json" } : {}),
      ...(options.headers || {}),
    },
  });
  const text = await response.text();
  let body = null;

  if (text) {
    try {
      body = JSON.parse(text);
    } catch {
      body = text;
    }
  }

  return { response, body };
}

async function serverIsListening() {
  try {
    const { response } = await fetchJson("/livez");
    return response.status === 200;
  } catch {
    return false;
  }
}

async function serverIsHealthy() {
  try {
    const { response, body } = await fetchJson("/api/health");
    return response.status === 200 && body?.status === "ok";
  } catch {
    return false;
  }
}

async function waitForHealth() {
  const deadline = Date.now() + START_TIMEOUT_MS;

  while (Date.now() < deadline) {
    if (await serverIsHealthy()) {
      return;
    }
    await sleep(HEALTH_POLL_MS);
  }

  fail(`Metabase did not become healthy at ${BASE_URL} within ${START_TIMEOUT_MS}ms`);
}

async function ensureServer() {
  if (await serverIsListening()) {
    log(`Using existing Metabase server at ${BASE_URL}`);
    await waitForHealth();
    return;
  }

  log(`Starting Metabase server with: ${START_COMMAND}`);
  startedServer = spawn(START_COMMAND, {
    cwd: process.cwd(),
    detached: process.platform !== "win32",
    shell: true,
    stdio: ["ignore", "pipe", "pipe"],
  });

  startedServer.stdout.on("data", (chunk) => {
    process.stdout.write(`[metabase] ${chunk}`);
  });
  startedServer.stderr.on("data", (chunk) => {
    process.stderr.write(`[metabase] ${chunk}`);
  });
  startedServer.on("exit", (code, signal) => {
    if (startedServer) {
      log(`Metabase server exited before test finished (code=${code}, signal=${signal})`);
    }
  });

  await waitForHealth();
}

function stopServer() {
  if (!startedServer) {
    return;
  }

  log("Stopping Metabase server started by this script");

  if (process.platform === "win32") {
    startedServer.kill("SIGTERM");
  } else {
    try {
      process.kill(-startedServer.pid, "SIGTERM");
    } catch {
      startedServer.kill("SIGTERM");
    }
  }

  startedServer = null;
}

function authHeaders() {
  const sessionId = process.env.MB_SESSION_ID || process.env.METABASE_SESSION_ID;
  const bearerToken = process.env.MB_MCP_BEARER_TOKEN || process.env.MB_BEARER_TOKEN;
  const apiKey = process.env.MB_API_KEY || process.env.METABASE_API_KEY;

  if (sessionId) {
    return { "X-Metabase-Session": sessionId };
  }

  if (bearerToken) {
    return { Authorization: `Bearer ${bearerToken}` };
  }

  if (apiKey) {
    return { "X-Api-Key": apiKey };
  }

  return null;
}

async function loginHeaders() {
  const username = process.env.MB_EMAIL || process.env.METABASE_EMAIL;
  const password = process.env.MB_PASSWORD || process.env.METABASE_PASSWORD;

  if (!username || !password) {
    return null;
  }

  const { response, body } = await fetchJson("/api/session", {
    method: "POST",
    body: JSON.stringify({ username, password }),
  });

  if (!response.ok || !body?.id) {
    fail(`Login failed for ${username}: HTTP ${response.status} ${JSON.stringify(body)}`);
  }

  return { "X-Metabase-Session": body.id };
}

async function getAuthHeaders() {
  const configuredHeaders = authHeaders();

  if (configuredHeaders) {
    return configuredHeaders;
  }

  const loggedInHeaders = await loginHeaders();

  if (loggedInHeaders) {
    return loggedInHeaders;
  }

  fail(
    [
      "Authentication is required for /api/mcp.",
      "Set MB_SESSION_ID, MB_MCP_BEARER_TOKEN, MB_API_KEY, or MB_EMAIL/MB_PASSWORD.",
    ].join(" "),
  );
}

function jsonRpcRequest(method, params = {}, id = 1) {
  return { jsonrpc: "2.0", method, params, id };
}

async function postJsonRpc(body, headers = {}) {
  const { response, body: responseBody } = await fetchJson("/api/mcp", {
    method: "POST",
    headers,
    body: JSON.stringify(body),
  });

  if (!response.ok) {
    fail(`MCP request failed: HTTP ${response.status} ${JSON.stringify(responseBody)}`);
  }

  if (responseBody?.error) {
    fail(`MCP JSON-RPC error: ${JSON.stringify(responseBody.error)}`);
  }

  return { response, body: responseBody };
}

async function initializeMcp(headers) {
  const { response } = await postJsonRpc(
    {
      jsonrpc: "2.0",
      method: "initialize",
      params: {
        clientInfo: { name: "metabase-mcp-contract-test", version: "0.1.0" },
      },
      id: 1,
    },
    headers,
  );

  const sessionId = response.headers.get("mcp-session-id");
  assert(sessionId, "initialize response did not include Mcp-Session-Id");

  await postJsonRpc(
    { jsonrpc: "2.0", method: "notifications/initialized", params: {} },
    { ...headers, "Mcp-Session-Id": sessionId },
  );

  return sessionId;
}

async function listTools(headers, sessionId) {
  const { body } = await postJsonRpc(
    jsonRpcRequest("tools/list", {}, 2),
    { ...headers, "Mcp-Session-Id": sessionId },
  );

  const tools = body?.result?.tools;
  assert(Array.isArray(tools), "tools/list did not return result.tools");
  return tools;
}

function validateTools(tools) {
  const ajv = new Ajv({ strict: false });
  const validators = new Map();

  for (const tool of tools) {
    assert(typeof tool.name === "string" && tool.name.length > 0, "tool missing name");
    assert(
      typeof tool.description === "string" && tool.description.length > 0,
      `${tool.name} missing description`,
    );
    assert(tool.inputSchema && typeof tool.inputSchema === "object", `${tool.name} missing inputSchema`);

    validators.set(tool.name, ajv.compile(tool.inputSchema));
  }

  return validators;
}

function validateArgs(validators, toolName, args) {
  const validate = validators.get(toolName);
  assert(validate, `tools/list did not include ${toolName}`);

  if (!validate(args)) {
    fail(`${toolName} arguments failed inputSchema validation: ${JSON.stringify(validate.errors)}`);
  }
}

async function callTool(headers, sessionId, validators, toolName, args, id) {
  validateArgs(validators, toolName, args);

  const { body } = await postJsonRpc(
    jsonRpcRequest("tools/call", { name: toolName, arguments: args }, id),
    { ...headers, "Mcp-Session-Id": sessionId },
  );

  const result = body?.result;
  assert(result, `${toolName} did not return result`);
  assert(!result.isError, `${toolName} returned tool error: ${result?.content?.[0]?.text}`);
  assert(result.content?.[0]?.type === "text", `${toolName} did not return text content`);

  try {
    return JSON.parse(result.content[0].text);
  } catch (error) {
    fail(`${toolName} returned non-JSON text: ${error.message}`);
  }
}

function colNames(data) {
  return (data?.cols || []).map((col) => col.name);
}

function assertJsonResult(label, result, expectedRowCount) {
  assert(result.status === "completed", `${label} did not complete`);
  assert(result.row_count === expectedRowCount, `${label} row_count expected ${expectedRowCount}, got ${result.row_count}`);
  assert(result.truncated === false, `${label} should not be truncated`);
  assert(Array.isArray(result.data?.cols), `${label} missing data.cols`);
  assert(Array.isArray(result.data?.rows), `${label} missing data.rows`);
  assert(!("csv" in result.data), `${label} json response should not include data.csv`);
  assert(!("pandas" in result.data), `${label} json response should not include data.pandas`);
}

function assertCsvResult(label, result, expectedRowCount) {
  assert(result.status === "completed", `${label} did not complete`);
  assert(result.row_count === expectedRowCount, `${label} row_count expected ${expectedRowCount}, got ${result.row_count}`);
  assert(result.truncated === false, `${label} should not be truncated`);
  assert(Array.isArray(result.data?.cols), `${label} missing data.cols`);
  assert(typeof result.data?.csv === "string", `${label} missing data.csv`);
  assert(!("rows" in result.data), `${label} csv response should not include data.rows`);
  assert(result.data?.pandas && typeof result.data.pandas === "object", `${label} missing data.pandas`);
  assert(result.data.pandas.dtype && typeof result.data.pandas.dtype === "object", `${label} missing data.pandas.dtype`);
  assert(Array.isArray(result.data.pandas.parse_dates), `${label} missing data.pandas.parse_dates`);
  assert(result.data.pandas.dtype.id === "string", `${label} expected pandas dtype for id to be string`);
  assert(result.data.pandas.dtype.name === "string", `${label} expected pandas dtype for name to be string`);
  assert(result.data.pandas.dtype.state === "string", `${label} expected pandas dtype for state to be string`);
  assert(
    result.data.pandas.parse_dates.includes("inserted_at") &&
      result.data.pandas.parse_dates.includes("updated_at"),
    `${label} expected pandas parse_dates to include inserted_at and updated_at`,
  );
}

function namesFromRows(result) {
  const namesIndex = colNames(result.data).indexOf("name");
  assert(namesIndex >= 0, "result did not include name column");
  return result.data.rows.map((row) => row[namesIndex]);
}

async function runContract() {
  await ensureServer();

  const headers = await getAuthHeaders();
  const sessionId = await initializeMcp(headers);
  const tools = await listTools(headers, sessionId);
  const validators = validateTools(tools);

  log(`Validated ${tools.length} MCP tool schemas from tools/list`);

  const searchArgs = {
    term_queries: ["warehouse"],
    semantic_queries: ["warehouse card or question"],
  };
  const searchResult = await callTool(headers, sessionId, validators, "search_cards", searchArgs, 3);
  const warehouseCard = searchResult.data?.find((card) => card.name === WAREHOUSE_CARD_NAME);
  assert(warehouseCard, `search_cards did not find ${WAREHOUSE_CARD_NAME}`);
  assert(warehouseCard.id, `${WAREHOUSE_CARD_NAME} card is missing id`);
  log(`Found ${WAREHOUSE_CARD_NAME} card id=${warehouseCard.id}`);

  const card = await callTool(
    headers,
    sessionId,
    validators,
    "get_card",
    { id: warehouseCard.id },
    4,
  );
  const stateParameter = card.parameters?.find((parameter) => parameter.slug === "state");
  assert(stateParameter?.id, `${WAREHOUSE_CARD_NAME} card is missing state parameter`);
  assert(card.database_id, `${WAREHOUSE_CARD_NAME} card is missing database_id`);

  const allJson = await callTool(
    headers,
    sessionId,
    validators,
    "execute_card",
    { id: warehouseCard.id, parameters: [], format: "json", limit: 20 },
    5,
  );
  assertJsonResult("Warehouses json", allJson, 8);

  const allCsv = await callTool(
    headers,
    sessionId,
    validators,
    "execute_card",
    { id: warehouseCard.id, parameters: [], format: "csv", limit: 20 },
    6,
  );
  assertCsvResult("Warehouses csv", allCsv, 8);

  const scheduledSql = [
    "select *",
    "from warehouses",
    "where schedule is not null",
    "  and schedule <> ''",
    "order by name",
  ].join("\n");

  const scheduledJson = await callTool(
    headers,
    sessionId,
    validators,
    "execute_native_query",
    {
      database_id: card.database_id,
      sql: scheduledSql,
      template_tags: null,
      parameters: [],
      format: "json",
      limit: 20,
    },
    7,
  );
  assertJsonResult("Scheduled warehouses json", scheduledJson, 4);
  assert(
    JSON.stringify(namesFromRows(scheduledJson)) ===
      JSON.stringify(["Dallas", "Sacramento", "Statesville", "Taunton"]),
    "Scheduled warehouses json returned unexpected names",
  );

  const scheduledCsv = await callTool(
    headers,
    sessionId,
    validators,
    "execute_native_query",
    {
      database_id: card.database_id,
      sql: scheduledSql,
      template_tags: null,
      parameters: [],
      format: "csv",
      limit: 20,
    },
    8,
  );
  assertCsvResult("Scheduled warehouses csv", scheduledCsv, 4);

  const stateParameters = [
    {
      id: stateParameter.id,
      type: stateParameter.type,
      target: ["variable", ["template-tag", "state"]],
      value: WAREHOUSE_STATE,
    },
  ];

  const stateJson = await callTool(
    headers,
    sessionId,
    validators,
    "execute_card",
    { id: warehouseCard.id, parameters: stateParameters, format: "json", limit: 20 },
    9,
  );
  assertJsonResult(`Warehouses state=${WAREHOUSE_STATE} json`, stateJson, 4);
  assert(
    JSON.stringify(namesFromRows(stateJson)) ===
      JSON.stringify(["Marketplace", "3PO", "Statesville", "United States"]),
    `Warehouses state=${WAREHOUSE_STATE} json returned unexpected names`,
  );

  const stateCsv = await callTool(
    headers,
    sessionId,
    validators,
    "execute_card",
    { id: warehouseCard.id, parameters: stateParameters, format: "csv", limit: 20 },
    10,
  );
  assertCsvResult(`Warehouses state=${WAREHOUSE_STATE} csv`, stateCsv, 4);

  log("MCP contract smoke test passed");
}

runContract()
  .catch((error) => {
    process.stderr.write(`MCP contract smoke test failed: ${error.stack || error.message}\n`);
    process.exitCode = 1;
  })
  .finally(() => {
    stopServer();
  });
