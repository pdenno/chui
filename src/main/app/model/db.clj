(ns app.model.db
  "Create and populate a database with schemas from various SDOs #{:CEFACT :OASIS :OAGI :ISO} 
   Every element (a named BBIE, BIE, data type etc.) has an :schema/term and a :schema-part/type.
   The :schema-part/type are #{:BCC :ACC :ASCC :CCT :uDT :qDT :BBIE :ABIE :ASBIE } cite{Kabak2010}.
   #{:ABIE :BBIE :ASBIE} have various 'contexts' (e.g. the business process context :Trade).
   :uDT and :qDT specify restrictions on :CCT (core component type) and value sets of :BCC. 
   Elements are organized into schema.
   Most access is through pathom, but some key functions for the REPL:
     (list-schemas) - Get the list of schema (their :mm/schema-name), which is a URN string.
     (get-schema <schema-urn-string>) - Get a map describing the whole schema. Its elements are :schema/content. 
     (get-term <schema-urn-string> <term>) - Get the map of information for the :schema/term.
     (get-term-schemas <term>) - Get a list of schema (their URN strings) for the term (a string)."
  (:require
   [app.model.db-util            :as du]
   [app.model.env                :as env]
   [app.model.util               :as util]
   [com.wsscode.pathom.connect   :as pc]
   [datahike.api                 :as d]
   [datahike.pull-api            :as dp]
   [mount.core                   :refer [defstate]]
   [clojure.tools.namespace.repl :as tools :refer [refresh]] ; for debugging
   [taoensso.timbre              :as log]))

(def db-cfg {:store {:backend :file :path "resources/database"}
             :rebuild-db? true
             :schema-flexibility :write})

(def diag (atom nil))
(defonce conn nil) ; "The connection to the database. It needs to be updated whenever you want to see the new state."

(def db-schema
  "Defines the datahike schema for this database.
     :db/db.cardinality=many means value is a vector of values of some :db.type."
  [#:db{:cardinality :db.cardinality/one,  :valueType :db.type/boolean, :ident :mm/file-not-read?}])

;;;================================ Communication with Clients =========================================
;;; I think the key idea here for pathom-mediated composabiltiy is for each resolver to rename all db/id 
;;; to context specific names. These are currently #{:sdb/schema-id :sdb/elem-id :sdb/imported-schema-id}.
;;; (The last one isn't just a schema but a prefix too.)
;;; The simplest composition then is implemented as a flat table with values and foreign key references.
;;; I think, however, you can 'go deep' in the ::pc/output and still maintain. See all-schema-ids-r.  
;;; See also person-resolver at Part 6, 43:26.
;;; For debugging

;;; Note that when you send an Ident, you get back a map with that ident and the response <=========
;;;(tryme [{[:schema/name "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2"] [:sdb/schema-id]}])
;;; ==> {[:schema/name "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2"] #:sdb{:schema-id 1230}}
(pc/defresolver schema-by-name-r [env {:schema/keys [name]}]
  {::pc/input #{:schema/name}
   ::pc/output [:sdb/schema-id]}
  {:sdb/schema-id (d/q `[:find ?e . :where [?e :schema/name ~name]] @conn)})

;;; This is based on the book. https://book.fulcrologic.com/#GoingRemote (See friends-resolver
;;; (tryme [{:message-schema [:list/id  {:list/schemas [:sdb/schema-id :schema/name]}]}]) ; RIGHT!
;;; (tryme [{[:list/id :message-schema] {:list/schemas [:sdb/schema-id :schema/name]}}])  ; WRONG! WHY?
(pc/defresolver list-r [env {:list/keys [id]}] ; e.g :list/id = :message-schema
  {::pc/input  #{:list/id}
   ::pc/output [{:list/schemas [:sdb/schema-id :schema/name]}]}
  (when (= id :message-schema)
    (when-let [schema-maps (->>
                            (d/q `[:find ?ent ?name ?topic
                                   :keys sdb/schema-id schema/name schema/topic
                                   :where
                                   [?ent :schema/name  ?name]
                                   [?ent :schema/topic ?topic]
                                   [?ent :schema/type ~id]]
                                 @conn)
                            (sort-by :schema/topic)
                            (mapv #(dissoc % :schema/topic))
                            not-empty)]
      {:list/id id
       :list/schemas schema-maps})))

(pc/defresolver message-schema-r [env input] ; THIS ONE WORKS (But...)
  {::pc/output [{:message-schema [:list/id {:list/schemas [:schema/name :sdb/schema-id]}]}]}
  {:message-schema {:list/id :message-schema}})

;;; (tryme [{[:sdb/schema-id 1229] [{:schema/message-sequence [{:sdb/elem-id [:schema-part/doc-string :schema-part/name :schema-part/type :schema-part/min-occurs :schema-part/max-occurs]}]}]}])

;;; (tryme [{[:sdb/schema-id 1229] [{:schema/message-sequence [:schema-part/doc-string :schema-part/name :schema-part/type :schema-part/min-occurs :schema-part/max-occurs]}]}])
;;; (tryme [{[:sdb/schema-id 1229] [:schema/name]}])
;;; (tryme [{[:sdb/schema-id 1229] [:schema/message-sequence]}])
(pc/defresolver schema-props-r [env {:sdb/keys [schema-id]}]
  {::pc/input #{:sdb/schema-id}
   ::pc/output [:schema/name :sdb/schema-id :schema/sdo :schema/type :schema/topic 
                :schema/subversion :schema/inline-typedefs :schema/spec
                {:schema/imported-schemas [:sdb/imported-schema-id]}
                {:schema/message-sequence [:sdb/elem-id]}]}
  (-> (dp/pull @conn '[*] schema-id) ; POD could also do the :keys thing on the pull. 
      (update :schema/message-sequence (fn [s] (mapv #(-> % (assoc :sdb/elem-id (:db/id %)) (dissoc :db/id)) s)))
      (update :schema/imported-schemas
              (fn [s]
                (mapv #(-> %
                           (assoc :sdb/imported-schema-id (:db/id %))
                           (dissoc :db/id)) s)))))

;;; (tryme [{[:sdb/elem-id 1280] [:schema/min-occurs]}])                          ; Simple
;;; (tryme [{[:schema/name tname] [{[:sdb/elem-id 1280] [:schema/min-occurs]}]}]) ; YES!
;;; (tryme [{[:schema/name tname] [{:schema/message-sequence [:schema-part/name :schema-part/type :schema-part/doc-string :schema-part/min-occurs :schema-part/max-occurs]}]}]) ; COMPLETE!
;;; (tryme [{[:sdb/schema-id 1230] [{:schema/message-sequence [:schema-part/name :schema-part/type :schema-part/doc-string :schema-part/min-occurs :schema-part/max-occurs]}]}]) ; COMPLETE! 
(pc/defresolver elem-props-r [env {:sdb/keys [elem-id]}]
  {::pc/input #{:sdb/elem-id}
   ::pc/output [:schema-part/doc-string
                :schema-part/name
                :schema-part/type
                :schema-part/min-occurs
                :schema-part/max-occurs]}
  (dp/pull @conn '[*] elem-id))

;;; Nice thing about pathom (relative to GraphQL) is that you don't have to start at the root.
;;; This has nothing to do with ::pc/input; you can add this to a query anywhere. 
(pc/defresolver current-system-time-r [_ _]
  {::pc/output [:server/time]}
  {:server/time (java.util.Date.)})

(def resolvers [schema-by-name-r
                schema-props-r
                elem-props-r
                list-r
                message-schema-r
                current-system-time-r])

(defn process-files! [])

;;;================================ Starting and Stopping ===========================================
;;; (user/restart) whenever you update the DB or the resolvers. (tools/refresh) if compilation fails.
(defn create-db!
  "Create the database if :rebuild? is true, otherwise just set the connection atom, conn."
  []
  (if (:rebuild-db? db-cfg)
    (binding [log/*config* (assoc log/*config* :min-level :info)]
      (d/delete-database db-cfg)
      (d/create-database db-cfg)
      (alter-var-root (var conn) (fn [_] (d/connect db-cfg)))
      (d/transact conn db-schema)
      (process-files!)
      (log/info "Created schema DB"))
    (alter-var-root (var conn) (fn [_] (d/connect db-cfg)))))

(defstate db
  :start
  (do (create-db!) db-cfg))
