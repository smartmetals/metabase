(ns metabase.agent-api.api-test
  "Agent API functional tests using session-based authentication.
   JWT and scope-related tests live in metabase-enterprise.agent-api.api-test."
  (:require
   [clojure.data.csv :as data.csv]
   [clojure.string :as str]
   [clojure.test :refer :all]
   [environ.core :as env]
   [java-time.api :as t]
   [medley.core :as m]
   [metabase.agent-api.api :as agent-api.api]
   [metabase.agent-api.settings :as agent-api.settings]
   [metabase.lib.core :as lib]
   [metabase.lib.metadata :as lib.metadata]
   [metabase.lib.normalize :as lib.normalize]
   [metabase.metabot.tools.util :as metabot.tools.u]
   [metabase.search.ingestion :as search.ingestion]
   [metabase.search.test-util :as search.tu]
   [metabase.session.models.session :as session.models]
   [metabase.test :as mt]
   [metabase.test.fixtures :as fixtures]
   [metabase.test.http-client :as client]
   [metabase.util :as u]
   [metabase.util.json :as json]
   [metabase.warehouse-schema.models.field-values :as field-values]
   [toucan2.core :as t2]))

(set! *warn-on-reflection* true)

(use-fixtures :once (fixtures/initialize :db :web-server :test-users))

(defn- orders-count-query
  "Create a simple count query on the orders table using lib functions."
  []
  (-> (lib/query (mt/metadata-provider)
                 (lib.metadata/table (mt/metadata-provider) (mt/id :orders)))
      (lib/aggregate (lib/count))))

;;; ------------------------------------------------- Session Auth Tests ------------------------------------------------

(deftest agent-api-session-token-auth-test
  (testing "Session tokens via X-Metabase-Session header authenticate successfully"
    (let [session-key (session.models/generate-session-key)
          _           (t2/insert! :model/Session
                                  {:id          (session.models/generate-session-id)
                                   :user_id     (mt/user->id :rasta)
                                   :session_key session-key})
          response    (client/client :get 200 "agent/v1/ping"
                                     {:request-options {:headers {"x-metabase-session" session-key}}})]
      (is (= {:message "pong"} response))))

  (testing "Invalid session token returns 401"
    (let [fake-session-key (str (random-uuid))
          response         (client/client :get 401 "agent/v1/ping"
                                          {:request-options {:headers {"x-metabase-session" fake-session-key}}})]
      ;; Invalid session means standard middleware doesn't set metabase-user-id,
      ;; so our middleware sees no auth and returns missing_authorization
      (is (= {:error   "missing_authorization"
              :message "Authentication required. Use X-Metabase-Session header or Authorization: Bearer <jwt>."}
             response)))))

(deftest agent-api-expired-session-test
  (testing "Expired sessions are rejected by the standard session middleware"
    ;; Set max-session-age to 1 minute for this test
    (with-redefs [env/env (assoc env/env :max-session-age "1")]
      (let [session-key (session.models/generate-session-key)
            old-time    (t/minus (t/instant) (t/minutes 2))]
        (mt/with-temp [:model/Session _ {:user_id     (mt/user->id :rasta)
                                         :session_key session-key
                                         :created_at  old-time}]
          (testing "Session older than max-session-age is rejected"
            (is (= {:error   "missing_authorization"
                    :message "Authentication required. Use X-Metabase-Session header or Authorization: Bearer <jwt>."}
                   (client/client :get 401 "agent/v1/ping"
                                  {:request-options {:headers {"x-metabase-session" session-key}}})))))))))

(deftest agent-api-enabled-setting-test
  (testing "External Agent API routes return 403 when disabled"
    (mt/with-temporary-setting-values [agent-api.settings/agent-api-enabled? false]
      (is (= "Agent API is not enabled."
             (mt/user-http-request :rasta :get 403 "agent/v1/ping"))))))

(deftest ai-features-enabled-setting-test
  (testing "External Agent API routes return 403 when AI features are globally disabled"
    (mt/with-temporary-raw-setting-values [:ai-features-enabled? "false"
                                           :agent-api-enabled?   "true"]
      (is (= "AI features are not enabled."
             (mt/user-http-request :rasta :get 403 "agent/v1/ping"))))))

;;; ------------------------------------------------- Functional Tests --------------------------------------------------

(deftest get-table-details-test
  (testing "Returns table details for valid table ID"
    (let [table-id (mt/id :orders)]
      (is (=? {:type           "table"
               :id             table-id
               :name           "ORDERS"
               :display_name   "Orders"
               :database_id    (mt/id)
               :fields         sequential?
               :related_tables sequential?}
              (mt/user-http-request :rasta :get 200 (str "agent/v1/table/" table-id))))))

  (testing "Returns 404 for non-existent table"
    (is (= "Not found."
           (mt/user-http-request :rasta :get 404 "agent/v1/table/999999"))))

  (testing "Respects query parameters"
    (let [table-id (mt/id :orders)]
      (is (=? {:type   "table"
               :id     table-id
               :fields empty?}
              (mt/user-http-request :rasta :get 200
                                    (str "agent/v1/table/" table-id "?with-fields=false&with-related-tables=false"))))))

  (testing "Field values are excluded by default"
    (let [table-id (mt/id :orders)
          table    (mt/user-http-request :rasta :get 200 (str "agent/v1/table/" table-id))]
      (is (every? #(nil? (:field_values %)) (:fields table)))))

  (testing "Field values are included when explicitly requested"
    (let [table-id (mt/id :orders)
          table    (mt/user-http-request :rasta :get 200 (str "agent/v1/table/" table-id "?with-field-values=true"))]
      (is (some #(seq (:field_values %)) (:fields table))))))

(deftest get-table-details-field-types-test
  (testing "Field metadata (base_type, effective_type, semantic_type, coercion_strategy) is returned correctly"
    (mt/with-temp [:model/Database {db-id :id}    {}
                   :model/Table    {table-id :id} {:db_id db-id, :name "t", :active true}
                   :model/Field    _              {:table_id table-id, :name "id"
                                                   :base_type :type/BigInteger
                                                   :semantic_type :type/PK}
                   :model/Field    _              {:table_id table-id, :name "name"
                                                   :base_type :type/Text}
                   :model/Field    _              {:table_id table-id, :name "created_at"
                                                   :base_type :type/Text
                                                   :effective_type :type/DateTime
                                                   :coercion_strategy :Coercion/ISO8601->DateTime}]
      (let [fields  (-> (mt/user-http-request :rasta :get 200 (str "agent/v1/table/" table-id))
                        :fields)
            by-name (m/index-by :name fields)]
        (testing "base_type is always set"
          (is (= "type/BigInteger" (get-in by-name ["id" :base_type])))
          (is (= "type/Text"       (get-in by-name ["name" :base_type])))
          (is (= "type/Text"       (get-in by-name ["created_at" :base_type]))))
        (testing "semantic_type is returned when set"
          (is (= "type/PK" (get-in by-name ["id" :semantic_type])))
          (is (nil? (get-in by-name ["name" :semantic_type]))))
        (testing "effective_type and coercion_strategy are returned when coerced"
          (is (= "type/DateTime"               (get-in by-name ["created_at" :effective_type])))
          (is (= "Coercion/ISO8601->DateTime" (get-in by-name ["created_at" :coercion_strategy]))))
        (testing "effective_type is omitted when it equals base_type"
          (is (not (contains? (get by-name "id") :effective_type)))
          (is (not (contains? (get by-name "name") :effective_type))))))))

(deftest get-metric-details-test
  (mt/with-temp [:model/Card metric {:name          "Test Metric"
                                     :type          :metric
                                     :database_id   (mt/id)
                                     :dataset_query (orders-count-query)}]
    (testing "Returns metric details for valid metric ID"
      (is (=? {:type                 "metric"
               :id                   (:id metric)
               :name                 "Test Metric"
               :queryable_dimensions sequential?}
              (mt/user-http-request :rasta :get 200 (str "agent/v1/metric/" (:id metric))))))

    (testing "Respects query parameters"
      (is (=? {:type "metric"
               :id   (:id metric)}
              (mt/user-http-request :rasta :get 200
                                    (str "agent/v1/metric/" (:id metric)
                                         "?with-queryable-dimensions=false&with-field-values=false")))))

    (testing "Returns 404 for non-existent metric"
      (is (= "Not found."
             (mt/user-http-request :rasta :get 404 "agent/v1/metric/999999"))))))

(defn- ensure-fresh-field-values!
  "Ensure field values exist for a field by deleting any existing ones and recreating them."
  [field-id]
  (t2/delete! :model/FieldValues :field_id field-id :type :full)
  (field-values/get-or-create-full-field-values! (t2/select-one :model/Field :id field-id)))

(defn- visible-field-id
  "Find the field-id string for a field by display name within a table's visible columns."
  [table-id field-display-name]
  (let [mp            (mt/metadata-provider)
        query         (lib/query mp (lib.metadata/table mp table-id))
        field-prefix  (metabot.tools.u/table-field-id-prefix table-id)
        visible-cols  (lib/visible-columns query)]
    (->> (keep-indexed (fn [i col]
                         (when (= (lib/display-name query col) field-display-name)
                           (str field-prefix i)))
                       visible-cols)
         first)))

(deftest get-table-field-values-test
  ;; Ensure field values exist for the field we'll test
  (ensure-fresh-field-values! (mt/id :people :state))

  (testing "Returns field statistics and values with default limit of 30"
    (let [table-id (mt/id :people)
          field-id (visible-field-id table-id "State")]
      (is (some? field-id) "Should find the State field")
      (let [result (mt/user-http-request :crowberto :get 200
                                         (format "agent/v1/table/%d/field/%s/values" table-id field-id))]
        (is (=? {:value_metadata {:statistics   {:distinct-count 49}
                                  :field_values sequential?}}
                result))
        (is (<= (count (:values result)) 30) "Should apply default limit of 30"))))

  (testing "Respects explicit limit parameter"
    (let [table-id (mt/id :people)
          field-id (visible-field-id table-id "State")]
      (is (=? {:value_metadata {:field_values #(= 5 (count %))}}
              (mt/user-http-request :crowberto :get 200
                                    (format "agent/v1/table/%d/field/%s/values?limit=5" table-id field-id))))))

  (testing "Returns 404 for non-existent table"
    (is (= "Not found."
           (mt/user-http-request :crowberto :get 404 "agent/v1/table/999999/field/t999999-0/values"))))

  (testing "Returns 400 for invalid field-id format"
    (let [table-id (mt/id :people)]
      (is (= "Invalid field_id format: not-a-valid-id"
             (mt/user-http-request :crowberto :get 400 (format "agent/v1/table/%d/field/not-a-valid-id/values" table-id)))))))

(deftest search-test
  (binding [search.ingestion/*force-sync* true]
    (search.tu/with-new-search-if-available-otherwise-legacy
      (mt/with-temp [:model/Table _ {:name "AgentSearchTestTable"}]
        (testing "Returns search results for term queries"
          (is (=? {:data        [{:type "table" :name "AgentSearchTestTable"}]
                   :total_count 1}
                  (mt/user-http-request :rasta :post 200 "agent/v1/search"
                                        {:term_queries ["AgentSearchTestTable"]}))))))))

(deftest coerce-query-list-test
  (let [coerce #'agent-api.api/coerce-query-list]
    (testing "arrays pass through unchanged"
      (is (= ["orders" "revenue"] (coerce ["orders" "revenue"]))))
    (testing "nil stays nil"
      (is (nil? (coerce nil))))
    (testing "a bare string becomes a single-element list"
      (is (= ["orders"] (coerce "orders"))))
    (testing "a JSON-stringified array of strings is unwrapped"
      (is (= ["orders" "revenue"] (coerce "[\"orders\", \"revenue\"]"))))
    (testing "JSON arrays with non-string elements are not unwrapped — they fall back to a literal single query so that downstream :sequential NonBlankString validation is never bypassed"
      (is (= ["[1, 2]"] (coerce "[1, 2]")))
      (is (= ["[\"\"]"] (coerce "[\"\"]"))))
    (testing "non-JSON strings become a single-element list"
      (is (= ["not json ["] (coerce "not json ["))))))

(defn- decode-query
  "Decode a base64-encoded query response to a Clojure map, then normalize it so lib functions work."
  [response]
  (-> response :query u/decode-base64 json/decode+kw lib.normalize/normalize))

(deftest construct-query-test
  (testing "Constructs a simple query from a table"
    (let [table-id (mt/id :orders)
          response (mt/user-http-request :rasta :post 200 "agent/v1/construct-query"
                                         {:table_id table-id})]
      (is (string? (:query response)) "Response should contain a query string")
      (let [decoded (decode-query response)]
        (is (= :mbql/query (lib/normalized-query-type decoded)))
        (is (= (mt/id) (lib/database-id decoded)))
        (is (= (mt/id :orders) (lib/primary-source-table-id decoded))))))

  (testing "Applies default limit of 200 when no limit is specified"
    (let [table-id (mt/id :orders)
          response (mt/user-http-request :rasta :post 200 "agent/v1/construct-query"
                                         {:table_id table-id})
          decoded  (decode-query response)]
      (is (= 200 (lib/current-limit decoded)))))

  (testing "Respects explicit limit"
    (let [table-id (mt/id :orders)
          response (mt/user-http-request :rasta :post 200 "agent/v1/construct-query"
                                         {:table_id table-id
                                          :limit    10})
          decoded  (decode-query response)]
      (is (= 10 (lib/current-limit decoded)))))

  (testing "Returns 404 for non-existent table"
    (is (= "No table found with table_id 999999"
           (mt/user-http-request :rasta :post 404 "agent/v1/construct-query"
                                 {:table_id 999999})))))

(deftest execute-query-test
  (testing "Default (csv) format returns slim shape with data.csv and trimmed cols"
    (let [table-id       (mt/id :orders)
          construct-resp (mt/user-http-request :rasta :post 200 "agent/v1/construct-query"
                                               {:table_id table-id
                                                :limit    5})
          execute-resp   (mt/user-http-request :rasta :post 200 "agent/v1/execute"
                                               {:query (:query construct-resp)})]
      (is (=? {:status    "completed"
               :row_count 5
               :truncated false
               :data      {:cols (fn [cols]
                                   (and (seq cols)
                                        (every? :name cols)
                                        (every? :base_type cols)
                                        (every? :display_name cols)
                                        (every? #(= #{:name :display_name :base_type} (set (keys %))) cols)))
                           :csv  string?}}
              execute-resp))
      (is (not (contains? (:data execute-resp) :rows)) "csv format should omit data.rows")
      (testing "Top-level keys are limited to the slim set"
        (is (= #{:status :row_count :truncated :running_time :started_at :data}
               (set (keys execute-resp)))))
      (testing "CSV header row is the column :name list"
        (let [first-line (first (str/split-lines (:csv (:data execute-resp))))
              col-names  (mapv :name (:cols (:data execute-resp)))]
          (is (= (str/join "," col-names) first-line))))))

  (testing "format=json returns data.rows and omits data.csv"
    (let [table-id       (mt/id :orders)
          construct-resp (mt/user-http-request :rasta :post 200 "agent/v1/construct-query"
                                               {:table_id table-id
                                                :limit    3})
          execute-resp   (mt/user-http-request :rasta :post 200 "agent/v1/execute"
                                               {:query (:query construct-resp) :format "json"})]
      (is (=? {:status    "completed"
               :row_count 3
               :truncated false
               :data      {:cols sequential?
                           :rows (fn [rows] (= 3 (count rows)))}}
              execute-resp))
      (is (not (contains? (:data execute-resp) :csv)) "json format should omit data.csv")))

  (testing "Soft cap clamps the constructed query's higher limit and sets truncated=true"
    ;; Default soft cap for csv is 500; constructing a query with limit=600 should still return 500 rows.
    (let [table-id       (mt/id :orders)
          construct-resp (mt/user-http-request :rasta :post 200 "agent/v1/construct-query"
                                               {:table_id table-id
                                                :limit    600})
          execute-resp   (mt/user-http-request :rasta :post 200 "agent/v1/execute"
                                               {:query (:query construct-resp)})]
      (is (=? {:status "completed" :row_count 500 :truncated true} execute-resp))))

  (testing "JSON soft cap is 200 (vs 500 for csv) and truncates accordingly"
    (let [table-id       (mt/id :orders)
          construct-resp (mt/user-http-request :rasta :post 200 "agent/v1/construct-query"
                                               {:table_id table-id
                                                :limit    600})
          execute-resp   (mt/user-http-request :rasta :post 200 "agent/v1/execute"
                                               {:query (:query construct-resp) :format "json"})]
      (is (=? {:status "completed" :row_count 200 :truncated true} execute-resp))))

  (testing "User-supplied limit above the format hard cap is clamped"
    (let [table-id       (mt/id :orders)
          construct-resp (mt/user-http-request :rasta :post 200 "agent/v1/construct-query"
                                               {:table_id table-id
                                                :limit    5000})
          execute-resp   (mt/user-http-request :rasta :post 200 "agent/v1/execute"
                                               {:query  (:query construct-resp)
                                                :format "json"
                                                :limit  10000})]
      ;; json hard cap is 500
      (is (=? {:status "completed" :row_count 500 :truncated true} execute-resp)))))

(deftest get-metric-field-values-test
  (ensure-fresh-field-values! (mt/id :orders :quantity))
  (mt/with-temp [:model/Card metric {:name          "Test Metric"
                                     :type          :metric
                                     :database_id   (mt/id)
                                     :dataset_query (orders-count-query)}]
    (testing "Returns field statistics for a field that has statistics"
      (let [metric-details (mt/user-http-request :rasta :get 200 (str "agent/v1/metric/" (:id metric)))
            quantity-field (m/find-first #(= (:name %) "QUANTITY") (:queryable_dimensions metric-details))]
        (is (some? quantity-field) "Quantity field should be in queryable_dimensions")
        (when-let [field-id (:field_id quantity-field)]
          (is (=? {:value_metadata {:statistics   map?
                                    :field_values sequential?}}
                  (mt/user-http-request :rasta :get 200
                                        (format "agent/v1/metric/%d/field/%s/values" (:id metric) field-id)))))))

    (testing "Returns 404 for non-existent metric"
      (is (= "Not found."
             (mt/user-http-request :rasta :get 404 "agent/v1/metric/999999/field/c999999-0/values"))))

    (testing "Returns 400 for field-id from wrong entity type"
      ;; Using a table field-id (t-prefix) when querying a metric should fail
      (is (re-find #"does not match expected prefix"
                   (mt/user-http-request :rasta :get 400 (format "agent/v1/metric/%d/field/t123-0/values" (:id metric))))))))

(deftest construct-metric-query-test
  (mt/with-temp [:model/Card metric {:name          "Test Metric"
                                     :type          :metric
                                     :database_id   (mt/id)
                                     :dataset_query (orders-count-query)}]
    (testing "Constructs a query from a metric"
      (let [response (mt/user-http-request :rasta :post 200 "agent/v1/construct-query"
                                           {:metric_id (:id metric)})]
        (is (string? (:query response)) "Response should contain a query string")
        (let [decoded (decode-query response)]
          (is (= :mbql/query (lib/normalized-query-type decoded)))
          (is (= (mt/id) (lib/database-id decoded))))))

    (testing "Returns 404 for non-existent metric"
      (is (= "Not found."
             (mt/user-http-request :rasta :post 404 "agent/v1/construct-query"
                                   {:metric_id 999999}))))))

(deftest construct-query-with-count-aggregation-test
  (testing "Count aggregation without field_id produces a valid query"
    (let [table-id (mt/id :orders)
          response (mt/user-http-request :rasta :post 200 "agent/v1/construct-query"
                                         {:table_id     table-id
                                          :aggregations [{:function "count"}]
                                          :limit        10})]
      (is (string? (:query response)))
      (let [decoded (decode-query response)]
        (is (= 1 (count (lib/aggregations decoded)))))))

  (testing "Count aggregation with field_id still works"
    (let [table-id (mt/id :orders)
          table    (mt/user-http-request :rasta :get 200 (str "agent/v1/table/" table-id))
          field-id (-> table :fields first :field_id)
          response (mt/user-http-request :rasta :post 200 "agent/v1/construct-query"
                                         {:table_id     table-id
                                          :aggregations [{:function "count" :field_id field-id}]
                                          :limit        10})]
      (is (string? (:query response)))
      (let [decoded (decode-query response)]
        (is (= 1 (count (lib/aggregations decoded))))))))

(deftest construct-query-with-filters-test
  (testing "Constructs a query with filters"
    (let [table-id (mt/id :orders)
          ;; Get table details to find a valid field_id
          table    (mt/user-http-request :rasta :get 200 (str "agent/v1/table/" table-id))
          field-id (-> table :fields first :field_id)
          response (mt/user-http-request :rasta :post 200 "agent/v1/construct-query"
                                         {:table_id table-id
                                          :filters  [{:field_id  field-id
                                                      :operation "is-not-null"}]
                                          :limit    10})]
      (is (string? (:query response)))
      (let [decoded (decode-query response)]
        (is (seq (lib/filters decoded)) "Query should have filters")))))

(deftest get-table-details-with-measures-test
  (let [measure-def (-> (lib/query (mt/metadata-provider)
                                   (lib.metadata/table (mt/metadata-provider) (mt/id :orders)))
                        (lib/aggregate (lib/sum (lib.metadata/field (mt/metadata-provider) (mt/id :orders :total)))))]
    (mt/with-temp [:model/Measure {measure-id :id} {:name       "Total Revenue"
                                                    :table_id   (mt/id :orders)
                                                    :definition measure-def}]
      (testing "with-measures=false (default) does not include measures"
        (let [table (mt/user-http-request :rasta :get 200 (str "agent/v1/table/" (mt/id :orders)))]
          (is (nil? (:measures table)))))

      (testing "with-measures=true includes measures for the table"
        (let [table (mt/user-http-request :rasta :get 200
                                          (str "agent/v1/table/" (mt/id :orders) "?with-measures=true"))]
          (is (sequential? (:measures table)))
          (is (=? [{:id   measure-id
                    :name "Total Revenue"}]
                  (:measures table))))))))

(deftest combined-query-test
  (testing "Returns results for a table query that fits in a single page"
    (let [table-id (mt/id :orders)
          field-id (visible-field-id table-id "ID")
          response (mt/user-http-request :rasta :post 202 "agent/v1/query"
                                         {:table_id table-id
                                          :order_by [{:field {:field_id field-id} :direction "asc"}]
                                          :limit    5})]
      (is (=? {:status             "completed"
               :row_count          5
               :continuation_token nil?
               :data               {:cols sequential?
                                    :rows (fn [rows] (= 5 (count rows)))}}
              response))))

  (testing "Continuation token returns next page of results when the total limit exceeds the page size"
    (let [table-id   (mt/id :orders)
          field-id   (visible-field-id table-id "ID")
          page-size  200
          total-rows 250
          page1      (mt/user-http-request :rasta :post 202 "agent/v1/query"
                                           {:table_id table-id
                                            :order_by [{:field {:field_id field-id} :direction "asc"}]
                                            :limit    total-rows})
          page2      (mt/user-http-request :rasta :post 202 "agent/v1/query"
                                           {:continuation_token (:continuation_token page1)})]
      (is (=? {:row_count          page-size
               :continuation_token string?
               :data               {:rows (fn [rows] (= page-size (count rows)))}}
              page1))
      (is (=? {:row_count          (- total-rows page-size)
               :continuation_token nil?
               :data               {:rows (fn [rows] (= (- total-rows page-size) (count rows)))}}
              page2))
      (is (not= (get-in page1 [:data :rows])
                (get-in page2 [:data :rows]))
          "Pages should return different rows")))

  (testing "No continuation_token when all rows are returned"
    (is (=? {:status             "completed"
             :continuation_token nil?}
            (mt/user-http-request :rasta :post 202 "agent/v1/query"
                                  {:table_id     (mt/id :orders)
                                   :aggregations [{:function "count"}]}))))

  (testing "Per-page cap limits a single page to 500 rows even when the total limit is higher"
    (is (=? {:status    "completed"
             :row_count (fn [n] (<= n 500))}
            (mt/user-http-request :rasta :post 202 "agent/v1/query"
                                  {:table_id (mt/id :orders)
                                   :limit    1000})))))

(defn- make-continuation-token [pagination]
  (-> {:query {:database (mt/id) :stages [{:source-table (mt/id :orders)}]}
       :pagination pagination}
      json/encode
      u/encode-base64))

(deftest continuation-token-validation-test
  (testing "Malformed pagination ints in a continuation token produce a 400, not a 500.
            This is robustness — the token isn't a trust boundary, since a caller can
            always issue a fresh program."
    (doseq [[label pagination] [["zero limit"         {:limit 0      :page 1}]
                                ["negative limit"     {:limit -10    :page 1}]
                                ["non-integer limit"  {:limit "lots" :page 1}]
                                ["zero page"          {:limit 200    :page 0}]
                                ["negative page"      {:limit 200    :page -1}]
                                ["non-integer page"   {:limit 200    :page "next"}]]]
      (testing label
        (mt/user-http-request :rasta :post 400 "agent/v1/query"
                              {:continuation_token (make-continuation-token pagination)})))))

(deftest combined-query-metric-test
  (mt/with-temp [:model/Card metric {:name          "Test Metric"
                                     :type          :metric
                                     :database_id   (mt/id)
                                     :dataset_query (orders-count-query)}]
    (testing "Returns results for a metric query"
      (is (=? {:status    "completed"
               :row_count pos?}
              (mt/user-http-request :rasta :post 202 "agent/v1/query"
                                    {:metric_id (:id metric)}))))))

(deftest search-finds-metrics-test
  (binding [search.ingestion/*force-sync* true]
    (search.tu/with-new-search-if-available-otherwise-legacy
      (mt/with-temp [:model/Card _metric {:name          "AgentSearchTestMetric"
                                          :type          :metric
                                          :database_id   (mt/id)
                                          :dataset_query (orders-count-query)}]
        (testing "Returns metrics in search results"
          (is (=? {:data        [{:type "metric" :name "AgentSearchTestMetric"}]
                   :total_count 1}
                  (mt/user-http-request :rasta :post 200 "agent/v1/search"
                                        {:term_queries ["AgentSearchTestMetric"]}))))))))

;;; ------------------------------------------------- Card & Database Tools ------------------------------------------

(deftest search-cards-test
  (binding [search.ingestion/*force-sync* true]
    (search.tu/with-new-search-if-available-otherwise-legacy
      (mt/with-temp [:model/Card _q {:name          "AgentSearchCardsQuestion"
                                     :type          :question
                                     :database_id   (mt/id)
                                     :dataset_query (orders-count-query)}
                     :model/Card _m {:name          "AgentSearchCardsModel"
                                     :type          :model
                                     :database_id   (mt/id)
                                     :dataset_query (orders-count-query)}]
        (testing "Returns saved questions matching a term query"
          (is (=? {:data        [{:type "question" :name "AgentSearchCardsQuestion"}]
                   :total_count 1}
                  (mt/user-http-request :rasta :post 200 "agent/v1/search/cards"
                                        {:term_queries ["AgentSearchCardsQuestion"]}))))
        (testing "Returns models with type=\"model\""
          (is (=? {:data        [{:type "model" :name "AgentSearchCardsModel"}]
                   :total_count 1}
                  (mt/user-http-request :rasta :post 200 "agent/v1/search/cards"
                                        {:term_queries ["AgentSearchCardsModel"]}))))))))

(deftest list-databases-test
  (testing "Returns databases the user can query, with native_permissions"
    (let [response (mt/user-http-request :rasta :get 200 "agent/v1/database")]
      (is (=? {:data        sequential?
               :total_count pos-int?}
              response))
      (let [app-db (first (filter #(= (:id %) (mt/id)) (:data response)))]
        (is (some? app-db) "The test database should be visible")
        (is (contains? #{"write" "none"} (:native_permissions app-db)))
        (is (string? (:name app-db)))
        (is (string? (:engine app-db)))))))

(deftest get-card-test
  (mt/with-temp [:model/Card {card-id :id}
                 {:name          "AgentGetCardTestQuestion"
                  :type          :question
                  :database_id   (mt/id)
                  :dataset_query (orders-count-query)}]
    (testing "Returns card details for an MBQL question"
      (is (=? {:id         card-id
               :type       "question"
               :name       "AgentGetCardTestQuestion"
               :query_type "query"
               :parameters []}
              (mt/user-http-request :crowberto :get 200 (str "agent/v1/card/" card-id)))))

    (testing "Non-existent card returns 404"
      (is (= "Not found."
             (mt/user-http-request :crowberto :get 404 "agent/v1/card/999999"))))))

(deftest get-card-native-query-test
  (let [native-query {:type     :native
                      :database (mt/id)
                      :native   {:query         "SELECT count(*) AS c FROM {{tbl}}"
                                 :template-tags {"tbl" {:name         "tbl"
                                                        :display-name "Table"
                                                        :type         :text
                                                        :required     true}}}}]
    (mt/with-temp [:model/Card {card-id :id}
                   {:name          "AgentGetCardTestNative"
                    :type          :question
                    :database_id   (mt/id)
                    :dataset_query native-query}]
      (testing "Native SQL is returned for a user with native query perms"
        (is (=? {:id         card-id
                 :query_type "native"
                 :native_query "SELECT count(*) AS c FROM {{tbl}}"
                 :native_template_tags [{:name         "tbl"
                                         :display_name "Table"
                                         :type         "text"
                                         :required     true}]}
                (mt/user-http-request :crowberto :get 200 (str "agent/v1/card/" card-id))))))))

(deftest execute-card-test
  (mt/with-temp [:model/Card {card-id :id}
                 {:name          "AgentExecuteCardTest"
                  :type          :question
                  :database_id   (mt/id)
                  :dataset_query (orders-count-query)}]
    (testing "Default (csv) format returns slim shape with data.csv"
      (let [resp (mt/user-http-request :rasta :post 200 (str "agent/v1/card/" card-id "/execute") {})]
        (is (=? {:status    "completed"
                 :row_count pos?
                 :truncated false
                 :data      {:cols (fn [cols] (and (seq cols)
                                                   (every? #(= #{:name :display_name :base_type} (set (keys %))) cols)))
                             :csv  string?}}
                resp))
        (is (not (contains? (:data resp) :rows)) "csv format should omit data.rows")))

    (testing "format=json returns data.rows"
      (let [resp (mt/user-http-request :rasta :post 200 (str "agent/v1/card/" card-id "/execute")
                                       {:format "json"})]
        (is (=? {:status "completed" :row_count pos? :data {:rows (fn [rows] (seq rows))}} resp))
        (is (not (contains? (:data resp) :csv)))))))

(deftest execute-card-with-parameters-test
  (testing "Native card with a template-tag parameter binds {id, type, value}"
    (mt/with-temp [:model/Card {card-id :id}
                   {:name          "AgentExecuteCardParamTest"
                    :type          :question
                    :database_id   (mt/id)
                    :dataset_query {:type     :native
                                    :database (mt/id)
                                    :native   {:query         "SELECT {{n}} AS n"
                                               :template-tags {"n" {:id           "tag-n"
                                                                    :name         "n"
                                                                    :display-name "N"
                                                                    :type         :number
                                                                    :required     true}}}}
                    :parameters    [{:id     "tag-n"
                                     :name   "n"
                                     :slug   "n"
                                     :type   "number/="
                                     :target [:variable [:template-tag "n"]]}]}]
      (let [resp (mt/user-http-request :crowberto :post 200 (str "agent/v1/card/" card-id "/execute")
                                       {:parameters [{:id "tag-n" :type "number/=" :value 42}]
                                        :format     "json"})]
        (is (=? {:status "completed" :row_count 1 :data {:rows [[42]]}} resp))))))

(deftest execute-native-query-test
  (testing "Default (csv) format returns slim shape"
    (let [resp (mt/user-http-request :crowberto :post 200 "agent/v1/native"
                                     {:database_id (mt/id)
                                      :sql         "SELECT 1 AS x"})]
      (is (=? {:status    "completed"
               :row_count 1
               :truncated false
               :data      {:cols (fn [cols] (every? #(= #{:name :display_name :base_type} (set (keys %))) cols))
                           :csv  string?}}
              resp))
      (is (not (contains? (:data resp) :rows)))))

  (testing "format=json returns rows as nested arrays"
    (is (=? {:status    "completed"
             :row_count 1
             :truncated false
             :data      {:rows [[1]]}}
            (mt/user-http-request :crowberto :post 200 "agent/v1/native"
                                  {:database_id (mt/id)
                                   :sql         "SELECT 1"
                                   :format      "json"}))))

  (testing "CSV quoting and null handling round-trip via clojure.data.csv/read-csv"
    (let [resp     (mt/user-http-request :crowberto :post 200 "agent/v1/native"
                                         {:database_id (mt/id)
                                          :sql         "SELECT 'a,b' AS c1, 'has \"quote\"' AS c2, CAST(NULL AS VARCHAR) AS c3"})
          csv-str  (-> resp :data :csv)
          decoded  (vec (data.csv/read-csv (java.io.StringReader. csv-str)))]
      (is (= [["C1" "C2" "C3"] ["a,b" "has \"quote\"" ""]] decoded))))

  (testing "Date/timestamp columns serialize to ISO 8601 in CSV"
    (let [resp    (mt/user-http-request :crowberto :post 200 "agent/v1/native"
                                        {:database_id (mt/id)
                                         :sql         "SELECT CAST('2026-04-26' AS DATE) AS d"})
          decoded (vec (data.csv/read-csv (java.io.StringReader. (-> resp :data :csv))))
          [_ [date-cell]] decoded]
      (is (re-matches #"2026-04-26(?:T00:00:00Z)?" date-cell)
          "date cell should be ISO 8601 (driver may include the zero time component)")))

  (testing "Rejects native query when the user lacks adhoc native perms"
    (mt/with-no-data-perms-for-all-users!
      (mt/user-http-request :rasta :post 403 "agent/v1/native"
                            {:database_id (mt/id)
                             :sql         "SELECT 1"}))))

(deftest cols->pandas-kwargs-test
  (let [f (var-get #'agent-api.api/cols->pandas-kwargs)]
    (testing "Numeric, text, boolean go in :dtype; temporal go in :parse_dates"
      (is (= {:dtype       {"ID"      "Int64"
                            "TOTAL"   "Float64"
                            "NAME"    "string"
                            "ACTIVE"  "boolean"}
              :parse_dates ["BORN" "CREATED_AT"]}
             (f [{:name "ID"         :base_type "type/BigInteger"}
                 {:name "TOTAL"      :base_type "type/Float"}
                 {:name "NAME"       :base_type "type/Text"}
                 {:name "ACTIVE"     :base_type "type/Boolean"}
                 {:name "BORN"       :base_type "type/Date"}
                 {:name "CREATED_AT" :base_type "type/DateTimeWithLocalTZ"}]))))
    (testing "isa? walks the hierarchy — semantic-type-ish descendants land in the right bucket"
      (is (= {:dtype       {"AMT"   "Float64"   ; type/Currency isa? type/Number
                            "EMAIL" "string"    ; type/Email isa? type/Text
                            "SCORE" "Float64"}  ; type/Score isa? type/Number
              :parse_dates ["TS"]}              ; type/CreationTimestamp isa? type/DateTime
             (f [{:name "AMT"   :base_type "type/Currency"}
                 {:name "EMAIL" :base_type "type/Email"}
                 {:name "SCORE" :base_type "type/Score"}
                 {:name "TS"    :base_type "type/CreationTimestamp"}]))))
    (testing "Unknown / unmapped types fall through to dtype \"string\""
      (is (= {:dtype {"X" "string"} :parse_dates []}
             (f [{:name "X" :base_type "type/Unknown"}]))))
    (testing "Bare type/Time stays as string (pandas can't parse a time-of-day cleanly)"
      (is (= {:dtype {"WHEN" "string"} :parse_dates []}
             (f [{:name "WHEN" :base_type "type/TimeWithLocalTZ"}]))))))

(deftest execute-pandas-hints-test
  (testing "data.pandas appears in CSV responses, omitted from JSON"
    (let [csv-resp  (mt/user-http-request :crowberto :post 200 "agent/v1/native"
                                          {:database_id (mt/id)
                                           :sql         "SELECT 1"
                                           :format      "csv"})
          json-resp (mt/user-http-request :crowberto :post 200 "agent/v1/native"
                                          {:database_id (mt/id)
                                           :sql         "SELECT 1"
                                           :format      "json"})]
      (is (=? {:data {:pandas {:dtype       map?
                               :parse_dates sequential?}}}
              csv-resp))
      (is (not (contains? (:data json-resp) :pandas)))))

  (testing "Mixed-type query routes columns into the correct buckets"
    ;; mt/user-http-request decodes JSON map keys as keywords, so dtype keys arrive as keywords
    ;; here even though they're strings on the wire.
    (let [resp (mt/user-http-request :crowberto :post 200 "agent/v1/native"
                                     {:database_id (mt/id)
                                      :sql         (str "SELECT CAST(1 AS INTEGER) AS i, "
                                                        "CAST(1.5 AS DOUBLE) AS f, "
                                                        "'x' AS t, "
                                                        "TRUE AS b, "
                                                        "DATE '2026-01-01' AS d, "
                                                        "CAST('2026-01-01 12:00:00' AS TIMESTAMP) AS dt")
                                      :format      "csv"})
          {:keys [dtype parse_dates]} (get-in resp [:data :pandas])
          parse-set (set (map name parse_dates))]
      (is (= "Int64"   (:I dtype)))
      (is (= "Float64" (:F dtype)))
      (is (= "string"  (:T dtype)))
      (is (= "boolean" (:B dtype)))
      (is (contains? parse-set "D")  "DATE column should be in parse_dates")
      (is (contains? parse-set "DT") "TIMESTAMP column should be in parse_dates")
      (is (not (contains? dtype :D)))
      (is (not (contains? dtype :DT)))))

  (testing "data.pandas is splat-shaped — keys match pd.read_csv kwargs (dtype, parse_dates)"
    (let [resp (mt/user-http-request :crowberto :post 200 "agent/v1/native"
                                     {:database_id (mt/id)
                                      :sql         "SELECT 1 AS x"
                                      :format      "csv"})]
      ;; Consumer pattern is `pd.read_csv(io.StringIO(data['csv']), **data['pandas'])` — so the
      ;; only top-level keys here must be valid read_csv kwarg names.
      (is (= #{:dtype :parse_dates}
             (set (keys (get-in resp [:data :pandas]))))))))

(deftest get-card-parameter-values-test
  (mt/with-temp
    [:model/Card {card-id :id}
     {:name          "AgentCardParamValuesTest"
      :type          :question
      :database_id   (mt/id)
      :dataset_query {:type     :query
                      :database (mt/id)
                      :query    {:source-table (mt/id :venues)}}
      :parameters    [{:id                   "_STATIC_CATEGORY_"
                       :name                 "Static Category"
                       :slug                 "static_category"
                       :type                 "category"
                       :values_source_type   "static-list"
                       :values_source_config {:values ["African" "American" "Asian"]}}]}]
    (testing "Returns the configured static values for the parameter"
      (let [response (mt/user-http-request :crowberto :get 200
                                           (str "agent/v1/card/" card-id "/params/_STATIC_CATEGORY_/values"))]
        (is (contains? response :values))
        (is (= #{"African" "American" "Asian"}
               (set (map first (:values response)))))))))
