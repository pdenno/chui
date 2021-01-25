(ns app.model.db-util
  "Utilities for Python AST (ugh!) and Datahike management"
  (:require
   [clojure.pprint        :as pp]
   [datahike.api          :as d]
   [datahike.pull-api     :as dp]))

(defn db-size
  "Return the number of datoms in the named database."
  [db-name]
  (let [conn (-> db-name d/connect deref)]
  (loop [i 1]
    (let [d (dp/pull conn '[*] i)]
      (if (< (count d) 2)
        (dec i)
        (recur (inc i)))))))

(defn print-db
  "Print the named db for debugging."
  [db-name & {:keys [schema?]}]
  (let [size (inc (db-size db-name))
        conn @(d/connect db-name)]
    (cond->> (dp/pull-many conn '[*] (range 1 size))
        (not schema?) (remove #(contains? % :db/valueType))
        true pp/pprint)))

(def demo-db "datahike:mem://demo")

(defmacro demo
  [pychunk query & {:keys [print-db?] :or {print-db? true}}]
  `(let [ast# (-> ~pychunk pyh/code2ast)
         db#  (small-db ast# :db-name "datahike:mem://demo")]
     (when ~print-db? (print-db "datahike:mem://demo"))
     ~query))

(defn learn-schema-from-data
  "Return an schema update with what is learned from the AST or examples."
  ([data] (learn-schema-from-data {} data))
  ([schema-map data]
   (cond (vector? data) (reduce (fn [sm v] (learn-schema-from-data sm v)) schema-map data)
         (map?    data) (reduce-kv (fn [sm k v]
                                     (cond (coll? v)
                                           (learn-schema-from-data
                                            (-> sm
                                                (assoc-in [k :db/cardinality] :db.cardinality/many)
                                                ;(assoc-in [k :db/unique]      :db.unique/identity) ; POD guessing.
                                                (assoc-in [k :db/valueType]   :db.type/ref))
                                            v)
                                           (contains? sm k) sm
                                           :else (cond-> sm
                                                   true          (assoc-in [k :db/cardinality] :db.cardinality/one)
                                                   (string?  v)  (assoc-in [k :db/valueType]   :db.type/string)
                                                   (float?   v)  (assoc-in [k :db/valueType]   :db.type/number)
                                                   (integer? v)  (assoc-in [k :db/valueType]   :db.type/number)
                                                   (keyword? v)  (assoc-in [k :db/valueType]   :db.type/keyword)
                                                   (boolean? v)  (assoc-in [k :db/valueType]   :db.type/boolean))))
                                   schema-map
                                   data)
         :else schema-map)))


(defn debug-schema
  "Fix some bugs in the schema. They originate in Python ASTs!"
  [schema-map]
  ;; Don't just fix the value. Tests might choke.
  schema-map)

(defn schema-map2schema
  [schema-as-map]
  (reduce-kv (fn [vv k v] (conj vv (assoc v :db/ident k)))
             []
             schema-as-map))

(def base-schema-map
  "This should not contain any types that will be learned from data." ; WELL, NOW IT DOES!
  {})

(defn small-db
  "Create the minimal db needed for the argument."
  [data-map & {:keys [db-name] :or {db-name "datahike:mem://small-db"}}]
  (let [schema-map (merge (learn-schema-from-data data-map) base-schema-map)]
    (d/delete-database db-name)  
    (d/create-database db-name :initial-tx (-> schema-map debug-schema schema-map2schema))
    (d/transact (d/connect db-name) data-map)
    db-name))

;;; This seems to cause problems in recursive resolution. (See resolve-db-id)"
(defn db-ref?
  "It looks to me that a datahike ref is a map with exactly one key: :db/id."
  [obj]
  (and (map? obj) (= [:db/id] (keys obj))))

;;; {:db/id 3779}
(defn resolve-db-id
  "Return the form with all its :db/id resolved."
  [form conn remove-db-ids?]
  (letfn [(resolve-aux [obj]
            (cond
              (db-ref? obj) (let [res (dp/pull @conn '[*] (:db/id obj))]
                              (if (= res obj) nil (resolve-aux res)))
              (map? obj) (reduce-kv (fn [m k v] (if (= k :db/id)
                                                  (if remove-db-ids? m (assoc m k v))
                                                  (assoc m k (resolve-aux v))))
                                    {}
                                    obj)
              (vector? obj)      (mapv resolve-aux obj)
              (set? obj)    (set (mapv resolve-aux obj))
              (coll? obj)        (map  resolve-aux obj)
              :else  obj))]
    (resolve-aux form)))


