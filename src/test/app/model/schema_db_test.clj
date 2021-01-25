(ns app.model.schema-db-test
  (:require
   [app.model.schema-db          :as sdb]
   [clojure.test         :refer  [deftest is testing]]
   [datahike.api                 :as d]
   [datahike.pull-api            :as dp]))

(def conn (d/connect sdb/db-cfg))

;;; Currently unused is (:cct/ObjectClass :cct/PrimitiveType :cct/UsageRule :cct/components
;;;  :cct/sc-id :cct/sc-type :cct/sc-use :cct/supplemental :component/doc-string :schema/codes)
;;; but then, some files aren't reading correctly. 
(deftest all-db-schema-props-used
  (testing "That every :db/ident of the sdb/db-schema is being used."
    (is (empty? (remove (fn [ident] (d/q `[:find ?name . :where [? ~ident ?name]] @conn))
                        (map :db/ident sdb/db-schema))))))
