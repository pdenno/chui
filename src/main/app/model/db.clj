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

;;; Owen's point: Black can't move a piece that would open up his own king to attack (the piece is "pinned"). 
;;;               Therefore white's king sometimes can move to places he could not otherwise move to.

(def db-cfg {:store {:backend :file :path "resources/database"}
             :rebuild-db? true
             :schema-flexibility :write})

(def diag (atom nil))
(defonce conn nil) ; "The connection to the database. It needs to be updated whenever you want to see the new state."

(def db-schema
  "Defines the datahike schema for this database.
     :db/db.cardinality=many means value is a vector of values of some :db.type."
   [#:db{:cardinality :db.cardinality/one,  :valueType :db.type/boolean, :ident :mm/file-not-read?}
    #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :square/id :unique :db.unique/identity}
    #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :square/player}
    #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :square/piece}
    #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :board/standard-start}])

;;;================================ Communication with Clients =========================================
;;; I think the key idea here for pathom-mediated composabiltiy is for each resolver to rename all db/id 
;;; to context specific names. These are currently #{:sdb/schema-id :sdb/elem-id :sdb/imported-schema-id}.
;;; (The last one isn't just a schema but a prefix too.)
;;; The simplest composition then is implemented as a flat table with values and foreign key references.
;;; I think, however, you can 'go deep' in the ::pc/output and still maintain. See all-schema-ids-r.  
;;; See also person-resolver at Part 6, 43:26.
;;; For debugging

(defn tryme [eql] ((var-get (resolve 'app.server.pathom/parser)) {} eql))

;;; (tryme [:game/state])
;;; (tryme [{:game/state [:square/id :square/piece :square/player]}]) ; same thing
;;; This one only returns information where there is a piece.
;;; THIS IS GOING TO HAVE TO CHANGE TO KNOW WHERE IN THE GAME WE ARE! (If I'm even going to keep it.)
(pc/defresolver game-state-r [_ _]
  {::pc/output [{:game/state [:square/id :square/piece :square/player]}]}
  {:game/state
   (d/q '[:find ?id ?piece ?player
          :keys square/id square/piece square/player
          :where
          [?x :square/id ?id]
          [?x :square/piece ?piece]
          [?x :square/player ?player]]
        @conn)})

;;; (tryme [:board/start]) ; Just gives IDs
;;; (tryme [{:board/start [:square/id :square/player :square/piece]}]) ; uses composition with square-start-r
(pc/defresolver board-start-r [_ _]
  {::pc/output [{:board/start [:square/id]}]}
  {:board/start
   (reduce (fn [r v] (conj r (first v)))
           [] 
           (d/q '[:find (pull ?x [:square/id #_#_:square/piece :square/player])
                  :where
                  [_ :board/standard-start ?x]]
                @conn))})

;;; (tryme [{:square/id :a1}])
;;; (tryme [{[:square/id :a1] [:square/piece :square/id :square/player]}])
(pc/defresolver square-start-r [_ {:square/keys [id]}]
  {::pc/input #{:square/id}
   ::pc/output [:square/id :square/piece :square/player]}
  (when-let [info (d/q `[:find ?piece ?player
                         :keys square/piece square/player
                         :where
                         [?x :square/id ~id]
                         [?x :square/piece ?piece]
                         [?x :square/player ?player]]
                       @conn)]
    (-> info first (assoc :square/id id))))

;;; Nice thing about pathom (relative to GraphQL) is that you don't have to start at the root.
;;; This has nothing to do with ::pc/input; you can add this to a query anywhere.
;;; (tryme [:server/time])
(pc/defresolver current-system-time-r [_ _]
  {::pc/output [:server/time]}
  {:server/time (java.util.Date.)})

(def resolvers [board-start-r
                square-start-r
                game-state-r
                current-system-time-r])

(defn process-data! []
  (d/transact conn [{:board/standard-start
                     (-> "resources/data/standard-start.edn" slurp read-string)}]))

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
      (process-data!)
      (log/info "Created schema DB"))
    (alter-var-root (var conn) (fn [_] (d/connect db-cfg)))))

(defstate db
  :start
  (do (create-db!) db-cfg))
