(ns app.model.schema-db
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
   [app.model.util               :as util :refer [xpath xpath- xml-type?]]
   [clojure.java.io              :as io]
   [clojure.string               :as str]
   [com.wsscode.pathom.connect   :as pc]
   [datahike.api                 :as d]
   [datahike.pull-api            :as dp]
   [mount.core                   :refer [defstate]]
   [clojure.tools.namespace.repl :as tools :refer [refresh]] ; for debugging
   [taoensso.timbre              :as log]))

;;; ToDo:
;;;    0) elem-props-r only works for UBL schema. 
;;;    1) UBL doesn't have :schema/doc-string on elements; the tag :component-name is the :schema-part/type. Investigate.
;;;    2) Get :schema/cc-type into everything. CEFACT schema have  <ccts:Acronym>BBIE</ccts:Acronym>
;;;    3) "/Users/pdenno/Documents/specs/OASIS/UBL-2.3/xsdrt/common/UBL-UnqualifiedDataTypes-2.3.xsd" is on bad files.
;;;    4) Consider separate properties for "libaries" (e.g. :library/content :library/sdo....) for code lists and components.
;;;       Note also that have currently :schema/message-sequence (for schema) and :schema/content (for libraries)
;;;       :schema/content is dissoc-ed in message schema, but not in "libraries"
;;;       Perhaps :schema/content should be called :xsd/content. Likewise :schema/xsd-ns
;;;       If going this route, it would be best to change all the 'sdo' like stuff to something else (maybe :sdo/)
;;;    5) Why is it called :schema/tag ? I think it should be called :xml/tag
;;;    6) Maybe the things currently called :schema-part/name in inline-schema should be called :schema-part/type-name <================
;;;       But keep :schema-part/name too! Hard to say, really, because there is already :schema-part/type. 
;;;       Maybe it should be :schema-typedef/type-name. 

(def db-cfg {:store {:backend :file :path "resources/database"}
             :rebuild-db? false
             :schema-flexibility :write})

(def diag (atom nil))
(defonce conn nil) ; "The connection to the database"
;;; POD make the following environment variables. 
(def ubl-root   "/Users/pdenno/Documents/specs/OASIS/UBL-2.3/xsdrt/")
(def oagis-root "/Users/pdenno/Documents/specs/OAGI/OAGIS_10_6_EnterpriseEdition/OAGi-BPI-Platform/org_openapplications_oagis/10_6/Model/")
(defonce bad-file-on-rebuild? (atom #{})) ; For debugging

;;; NB Not everything that is :db.cardinality/many is :db.type/ref. For example, some are strings.
;;;    Not every :db.type/ref is many. For example, :schema/ccts-data is a single map. 
(def db-schema
  "Defines the datahike schema for this database.
     :db/db.cardinality=many means value is a vector of values of some :db.type."
  [#:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/Cardinality}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :cct/CategoryCode}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/DataTypeTermName}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/Definition}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/Description}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/DictionaryEntryName}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/Name}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/ObjectClass}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :cct/PrimitiveType}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/PropertyTermName}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/QualifierTerm}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/RepresentationTermName}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/UniqueID}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/UsageRule}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/VersionID}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :cct/components}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/sc-id}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/sc-type}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/sc-use}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :cct/supplemental}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :iso/CodeName, :doc "Used in OAGIS/ISO currency codes"}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :library/content}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :mm/comment :doc "All the mm things are for debugging."}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :mm/debug}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/boolean, :ident :mm/file-not-read?}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :mm/pod-temp-include}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :mm/zip-keys}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :mm/zip-vals}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :schema/cc-type}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/ref,     :ident :schema/ccts-data}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/code-list-name}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/code-lists}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/ref,     :ident :schema/codes}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/complex-types}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/doc-string}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :schema/enum-vals}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/imported-schemas}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/inline-component-name}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/inline-typedefs}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/message-sequence}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/name, :unique :db.unique/identity}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/pathname}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/prefix}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/referenced-schema}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/restriction}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :schema/sdo}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/short-name}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/simple-types}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :schema/spec}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/subversion}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/term, :doc "An ID unique within the schema"}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/topic}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :schema/type}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/ref,     :ident :schema/type-attrs}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/type-name}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema-part/doc-string}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :schema-part/max-occurs}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :schema-part/min-occurs}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema-part/name}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema-part/type}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :xsd/choice}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :xsd/extension}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/fractionDigits}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/maxLength}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/minInclusive}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/minLength}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :xsd/pattern}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :xsd/restriction}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/totalDigits}])

(def xsd-number?
  "The set of XSD constraints types that we know about"
  #{:xsd/totalDigits :xsd/fractionDigits :xsd/minLength :xsd/maxLength :xsd/minInclusive})

(def xsd-constraint?
  "The set of XSD constraints types that we know about"
  (conj xsd-number? :xsd/pattern))

(def oagis2ccts-key-map
  "Translate names of properties found in OAGIS to the equivalent used in the database."
  (let [key-names (->> db-schema (map :db/ident) (filter #(= (namespace %) "cct")))
        oagis-names (map #(keyword "ROOT" (str "ccts_" (name %))) key-names)]
    (zipmap oagis-names key-names)))

(defn read-xsd-type
  [tag val]
  (if (xsd-number? tag) (read-string val) val))

(defn schema-ns
  "Return the namespace urn string for the argument xmap."
  [xmap]
  (-> (xpath xmap :xsd/schema) :xml/attrs :targetNamespace))

(defn schema-sdo
  "Return a keyword identifying the XML file's standards development organization."
  [xmap]
  (if-let [ns (schema-ns xmap)]
    (cond (re-matches #"^urn:oasis:[\w,\-,\:]+:ubl:[\w,\-,\:]+$" ns) :oasis,
          (re-matches #"^urn:un:unece:uncefact:[\w,\-,\:]+$" ns) :cefact, ; cefact via UBL ; POD NONE OF THESE???
          (re-matches #"^http://www.openapplications.org/oagis/.*$" ns) :oagi,
          (re-matches #"^urn:iso:std:iso:20022:tech:xsd:pain.+$" ns) :iso) ; ISO via oagis
    (log/warn "Cannot determine file SDO:" (:schema/pathname xmap))))

(def non-standard-schema-topics
  "Easiest to just define these explicitly"
  {"urn:iso:std:iso:20022:tech:xsd:pain.001.001.04"                              "Datatypes, Financial",                       
   "urn:iso:std:iso:20022:tech:xsd:pain.001.001.05"                              "Datatypes, Financial", 
   "urn:iso:std:iso:20022:tech:xsd:pain.002.001.04"                              "Datatypes, Financial",
   "urn:iso:std:iso:20022:tech:xsd:pain.002.001.05"                              "Datatypes, Financial", 
   "urn:iso:std:iso:20022:tech:xsd:pain.008.001.03"                              "Datatypes, Financial", 
   "urn:iso:std:iso:20022:tech:xsd:pain.008.001.04"                              "Datatypes, Financial",
   "urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2"    "Components, CommonExtensions",
   "urn:oagis-10.6:CodeList_DateTimeFormatCode_1.xsd"                            "Codelist, DateTimeFormats"
   "urn:oagis-10.6:CodeList_TimeZoneCode_1.xsd"                                  "Codelist, TimeZones",
   "urn:oagis-10.6:CodeList_DateFormatCode_1.xsd"                                "Codelist, DateFormat",
   "urn:oagis-10.6:CodeList_CharacterSetCode_IANA_20131220.xsd"                  "Codelist, CharacterSets",
   "urn:oasis:names:specification:ubl:schema:xsd:UnqualifiedDataTypes-2"         "Datatypes, Unqualified",
   "urn:oagis-10.6:CodeList_CurrencyCode_ISO_7_04.xsd"                           "Codelist, Currencies",
   "urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2"        "Components, CommonBasic",
   "urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2"    "Components, CommonAggregate",
   "urn:oagis-10.6:CodeLists_1.xsd"                                              "Codelist,  Aggregated",
   "urn:oagis-10.6:CodeList_ConditionTypeCode_1.xsd"                             "Codelist, ConditionTypes", 
   "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2"    "Components, Core",
   "urn:oasis:names:specification:ubl:schema:xsd:QualifiedDataTypes-2"           "Datatypes, Qualified"
   "urn:oasis:names:specification:ubl:schema:xsd:CommonSignatureComponents-2"    "Components, CommonSignature",
   "urn:oagis-10.6:CodeList_ConstraintTypeCode_1.xsd"                            "Codelist, ConstraintTypes",
   "urn:oagis-10.6:CodeList_TimeFormatCode_1.xsd"                                "Codelist, TimeFormats"})

(defn q-schema-topic
  "Lookup the SDO for a schema in the DB."
  [urn]
  (d/q `[:find ?topic .
         :where [?s :schema/name ~urn]
         [?s :schema/topic ?topic]] @conn))

(defn q-schema-sdo
  "Lookup the SDO for a schema in the DB."
  [urn]
  (d/q `[:find ?sdo .
         :where [?s :schema/name ~urn]
         [?s :schema/sdo ?sdo]] @conn))

(defn q-schema-type
  "Lookup the SDO for a schema in the DB."
  [urn]
  (d/q `[:find ?type .
         :where [?s :schema/name ~urn]
         [?s :schema/type ?type]] @conn))

(defn schema-topic
  "Return the portion of the URN that most specifically describes the schema content.
   This is not necessarily unique!"
  [urn]
  (let [desc [(q-schema-sdo urn) (q-schema-type urn)]]
     (cond (= desc [:oasis :message-schema])
           (->> urn (re-matches #"^urn:oasis:names:specification:ubl:schema:xsd:(.+)-\d$") second),
           (= desc [:oagi :message-schema])
           (->> urn (re-matches #"^urn:oagis-\d+\.\d+:(.+)$") second),
           (contains? non-standard-schema-topics urn)
           (get non-standard-schema-topics urn), 
           :else
           (log/warn "Cannot determine schema topic" urn))))

(defn schema-spec
  "Return a keyword signifying the specification of which the schema is part.
   These are #{:ubl-2, :oagis-10, etc.}. It is used as :schema/spec"
  [xmap]
  (let [sdo (:schema/sdo xmap)
        ns  (schema-ns xmap)]
    (cond (= sdo :cefact)
          (cond (= ns "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2")
                :cefact-ccl ; POD since we already have the :sdo, do we need "cc-"
                :else (log/warn "Cannot determine file spec:" (:schema/pathname xmap)))
          (= sdo :oasis)
          (cond (re-matches #"^urn:oasis:[\w,\-,\:]+:ubl:[\w,\-,\:]+(\-2)$" ns)
                :ubl-2
                :else (log/warn "Cannot determine file spec:" (:schema/pathname xmap)))
          (= sdo :oagi)
          (cond (re-matches #"^http://www.openapplications.org/oagis/10$" ns)
                :oagis-10,
                :else (log/warn "Cannot determine file spec:" (:schema/pathname xmap)))
          (= sdo :iso) :iso-20022
          :else (log/warn "Cannot determine file spec:" (:schema/pathname xmap)))))

(defn schema-subversion
  "Return the subversion. NB: Currently this is hard coded. Should be an environment variable." ; POD need env. 
  [xmap]
  (let [spec (:schema/spec xmap)]
    (cond (= spec :ubl-2) "3"
          (= spec :oagis-10) "6"
          (= spec :iso-20022)  "ToDo"
          (= spec :cefact-ccl) "ToDo"))) ; POD ToDo maybe. See also UBL CCL. 
  
(defn schema-name
  "Return the name of the schema object. This uses the XML content to determine one."
  [xmap]
  (let [sdo (:schema/sdo xmap)]
    (cond 
      (= :oagi sdo)
      (if-let [name  (-> (xpath xmap :xsd/schema :xsd/element) :xml/attrs :name)]
        (str "urn:oagis-10.6:" name) ; POD 10.6
        (let [sep   (-> env/env :file-separator re-pattern)]
          (if-let [pname (-> xmap :schema/pathname (str/split sep) last)]
             (do (log/warn "Using pathname to define OAGIS schema name:" pname)
                 (str "urn:oagis-10.6:" pname)) ; POD 10.6
              (do (log/warn "Could not determine OAGIS schema name.")
                  :mm/nil)))), 
      (#{:oasis :cefact :iso} sdo)
      (if-let [name (schema-ns xmap)]
        name
        (log/warn "Could not determine UBL or ISO schema name:" (:schema/pathname xmap))))))

(defn schema-type
  "Return a keyword signifying the specification of which the schema is part.
   These are #{:ubl-2, :oagis-10, etc.}. It is used as :schema/spec"
  [xmap]
  (let [sdo (:schema/sdo xmap)
        ns  (schema-ns xmap)]
    (cond (= sdo :cefact)
          (cond (= ns "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2")
                :unqualified-datatypes
                :else (log/warn "Cannot determine file spec:" (:schema/pathname xmap)))
          (= sdo :oasis)
          (cond (= ns "urn:oasis:names:specification:ubl:schema:xsd:UnqualifiedDataTypes-2") ; POD -2
                :unqualified-datatypes,
                (= ns "urn:oasis:names:specification:ubl:schema:xsd:QualifiedDataTypes-2")
                :qualified-datatypes,
                (re-matches #"^urn:oasis:names:specification:ubl:schema:xsd:Common[\w,\-,\:]+Components\-2$" ns)
                :component-types
                (re-matches #"^urn:oasis:names:specification:ubl:schema:xsd:\w+-2$" ns)
                :message-schema
                :else (log/warn "Cannot determine file spec:" (:schema/pathname xmap)))
          (= sdo :oagi)
          (cond (re-matches #".+CodeLists.+" (:schema/pathname xmap))
                :code-list
                (re-matches #".+Components.+" (:schema/pathname xmap))
                :component-types
                (re-matches #"^http://www.openapplications.org/oagis/10$" ns)
                :message-schema ; POD Not quite correct. 
                :else (log/warn "Cannot determine schema-type:" (:schema/pathname xmap)))
          (= sdo :iso) :iso-20022-schema)))

(defn imported-schemas
  "Using the :xsd/import, return a map of the prefixes used in the schema."
  [xmap]
  (let [ischemas
        (->>
         (xpath xmap :xsd/schema)
         :xml/content
         (filter #(xml-type? % :xsd/import))
         (map #(-> % :xml/attrs :namespace)))
        ns-info (:xml/ns-info xmap)]
    (reduce (fn [res schema]
               (if-let [prefix (-> ns-info :u->ps (get schema) first)]
                 (conj res {:schema/prefix prefix :schema/referenced-schema schema})
                 (do (log/warn "No prefix for schema" schema) res)))
            []
            ischemas)))

(defn read-clean
  "Return a map structure containing the :xml/content (cleaned-up) and :ns-info."
  [pathname]
  (let [xml (util/read-xml pathname)]
    (as-> xml ?xmap
        (assoc ?xmap :schema/sdo (schema-sdo ?xmap))
        (assoc ?xmap :schema/spec (schema-spec ?xmap))
        (assoc ?xmap :schema/subversion (schema-subversion ?xmap))
        (assoc ?xmap :schema/type (schema-type ?xmap))
        (assoc ?xmap :schema/name (schema-name ?xmap)))))

;;;=======================  Rewrite xsd to schema content for DB  ===========================
(defn rewrite-xsd-dispatch
  [obj & [tag]]
   (let [schema-type (:schema/type obj)
         schema-sdo  (:schema/sdo obj)
         meth
         (cond (keyword? tag) tag, 
               (#{:unqualified-datatypes :qualified-datatypes :component-types} schema-type) schema-type,
               (and (= schema-type :message-schema) (= schema-sdo :oasis)) :ubl-message-schema,
               (and (= schema-type :message-schema) (= schema-sdo :oagi))  :oagis-message-schema,
               (= schema-type :code-list) :code-list-schema
               (= schema-type :iso-20022-schema) :iso-20022-schema
               ;;(and (string? schema-name) (re-matches #"^urn:oasis.*Common.*Components.*" schema-name)) :component-schema,
               (and (map? obj) (contains? obj :schema/tag)) (:schema/tag obj),
               (and (map? obj) (contains? obj :ref)) :ref
               (contains? obj :schema/tag) (:schema/tag obj))] ; POD this one for polymorphism #{:xsd/element :xsd/choice} etc.
     meth))

(defmulti rewrite-xsd #'rewrite-xsd-dispatch)

(defmethod rewrite-xsd nil [obj & schema]
  (if schema
    (log/warn "No method for obj/schema.")
    (log/warn "No method for obj."))
  (reset! diag {:obj obj :schema schema})
  :mm/rewrite-xsd-nil-method)

(defmethod rewrite-xsd :xsd/extension 
  [obj & _]
  (-> obj :xml/attrs :base))

(defmethod rewrite-xsd :xsd/restriction
  [obj & _]
  (-> obj :xml/attrs :base))

(defmethod rewrite-xsd :cc-supplemental
  [xsd-attr & _]
  (assert (xml-type? xsd-attr :xsd/attribute))
  (as-> {} ?m
      (assoc ?m :cct/sc-id   (-> xsd-attr :xml/attrs :name))
      (assoc ?m :cct/sc-type (-> xsd-attr :xml/attrs :type))
      (assoc ?m :cct/sc-use  (-> xsd-attr :xml/attrs :use))
      (reduce (fn [m attr] (assoc m (:schema/tag attr) (:xml/content attr)))
              ?m
              (-> (xpath xsd-attr :xsd/annotation :xsd/documentation) :xml/content))))

(defn uq-dt-common
  "Process common parts of CEFACT and OASIS unqualified datatypes."
  [dt-xml]
  (assert (xml-type? dt-xml :xsd/complexType))
  (as-> {} ?dt
    (assoc ?dt :schema/term (-> dt-xml :xml/attrs :name))
    (reduce (fn [m attr] (assoc m (:schema/tag attr) (:xml/content attr)))
            ?dt
            (-> (xpath dt-xml :xsd/annotation :xsd/documentation) :xml/content))
    (let [extend-restrict (if (xpath dt-xml :xsd/simpleContent :xsd/extension) :xsd/extension :xsd/restriction)]
      (assoc ?dt :cct/supplemental
             (-> {} 
               (assoc extend-restrict (rewrite-xsd (xpath dt-xml :xsd/simpleContent extend-restrict)))
               (assoc :cct/components
                      (mapv (fn [sc] (rewrite-xsd sc :cc-supplemental))
                            (-> (xpath dt-xml :xsd/simpleContent extend-restrict) :xml/content))))))))
  
;;; Toplevels for some schema:
;;;   "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2"
;;;   "urn:oasis:names:specification:ubl:schema:xsd:UnqualifiedDataTypes-2"
(defmethod rewrite-xsd :unqualified-datatypes [xmap]
  (-> xmap
      (assoc :library/content (mapv uq-dt-common
                               (filter #(xml-type? % :xsd/complexType)
                                       (-> (xpath xmap :xsd/schema) :xml/content))))
      (dissoc :xml/ns-info :xml/content))) ; POD remove :xml/content from this dissoc if you aren't using :library/content

;;; POD currently one UBL!
;;; POD maybe rewrite this to have something like namespace for the references "udt". "schema/referenced-schema
(defn q-dt-common
  "Process one qualified datatype schema element."
  [dt-xml]
  (assert (and (xml-type? dt-xml :xsd/complexType) (= 1 (-> dt-xml :xml/content count))))
  (if-let [restrict (xpath dt-xml :xsd/simpleContent :xsd/restriction)]
    (as-> {} ?dt
      (assoc ?dt :schema/term (-> dt-xml :xml/attrs :name))
      (assoc ?dt :schema/restriction (rewrite-xsd restrict :xsd/restriction))) ; POD See dead code, 2020-11-15
    (log/warn "Does not look like a qualified datatype schema element.")))

(defmethod rewrite-xsd :qualified-datatypes [xmap & _]
  (-> xmap
      (assoc :library/content (mapv q-dt-common
                               (filter #(xml-type? % :xsd/complexType)
                                       (-> (xpath xmap :xsd/schema) :xml/content))))
      (assoc :schema/imported-schemas (imported-schemas xmap))
      (assoc :mm/comment "The :schema/imported-schema is used to lookup the type being restricted.")
      (dissoc :xml/ns-info :xml/content))) ; POD remove :xml/content from this dissoc if you aren't using :library/content

(defmethod rewrite-xsd :component-types
  [xmap & _]
  (let [elems (->> (xpath xmap :xsd/schema)
                   :xml/content
                   (filter #(xml-type? % :xsd/element)))]
    (-> xmap
        (assoc :library/content (mapv (fn [elem]
                                   (-> {}
                                       (assoc :schema/cc-type :BBIE) ; POD I think!
                                       (assoc :schema/term (-> elem :xml/attrs :name))
                                       (assoc :schema/type-name (-> elem :xml/attrs :type))))
                                 elems))
        (assoc :schema/imported-schemas (imported-schemas xmap)) ; POD was :schema/referencec-schemas
        (dissoc :xml/ns-info :xml/content))))

(defmethod rewrite-xsd :xsd/element ; POD was :typical-elem
  [xelem & _]
  (assert (and (xml-type? xelem :xsd/element)
               (or (-> xelem :xml/attrs :ref)     ; UBL
                   (-> xelem :xml/attrs :name)))) ; ISO/OAGIS
  (let [attrs (:xml/attrs xelem)]
    (cond-> {}
      (:ref  attrs)      (assoc :schema-part/name (:ref  attrs))
      (:name attrs)      (assoc :schema-part/name (:name attrs))
      (:type attrs)      (assoc :schema-part/type (:type attrs))
      (:minOccurs attrs) (assoc :schema-part/min-occurs (-> attrs :minOccurs keyword))
      (:maxOccurs attrs) (assoc :schema-part/max-occurs (-> attrs :maxOccurs keyword)))))

(defmethod rewrite-xsd :xsd/choice
  [xchoice & _]
  {:xsd/choice (mapv #(rewrite-xsd %) (:xml/content xchoice))})
                     
(defmethod rewrite-xsd :ubl-message-schema
  [xmap & _]
  (as-> xmap ?x
      (assoc ?x :schema/imported-schemas (imported-schemas ?x))
      (assoc ?x :schema/short-name (-> (xpath ?x :xsd/schema :xsd/complexType) :xml/attrs :name)) ; POD BUG <=======================
      (if-let [typedefs (not-empty (filter #(xml-type? % :xsd/complexType) (-> (xpath ?x :xsd/schema) :xml/content)))]
        (assoc ?x :schema/inline-typedefs 
               (mapv #(rewrite-xsd % :inline-typedef) typedefs))
        ?x)
      (if-let [elems (not-empty (filter #(xml-type? % :xsd/element)
                                        (:xml/content (xpath ?x :xsd/schema))))]
        (assoc ?x :schema/message-sequence (mapv #(rewrite-xsd % :xsd/element) elems))
        ?x)
      (dissoc ?x :xml/ns-info :xml/content)))

;;; POD: This is doing duty for both the message elements, and the elements within inline complexTypes. OK?
(defmethod rewrite-xsd :oagis-message-elem
  [xelem & _]
  (let [attrs (:xml/attrs xelem)]
    (cond-> (if-let [doc (:xml/content (xpath xelem :xsd/annotation :xsd/documentation))]
              {:schema/doc-string doc}
              {})
      true               (assoc :schema-part/name (or (:name attrs) (:ref attrs)))
      (:type attrs)      (assoc :schema-part/type (:type attrs))
      (:minOccurs attrs) (assoc :schema-part/min-occurs (-> attrs :minOccurs keyword))
      (:maxOccurs attrs) (assoc :schema-part/max-occurs (-> attrs :maxOccurs keyword)))))

(defmethod rewrite-xsd :oagis-type-attr
  [xsd-attr & _]
  (let [attr (:xml/attrs xsd-attr)]
    (cond-> {}
      true         (assoc :schema-part/name (:name attr))
      (:type attr) (assoc :schema-part/type (:type attr))
      (= (:use attr) "optional") (assoc :schema-part/min-occurs :0)
      (= (:use attr) "optional") (assoc :schema-part/max-occurs :1))))

(defmethod rewrite-xsd :oagis-ccts-def
  ;; Argument is a vector of :ROOT/ccts_whatever properties.
  ;; Return a msp of these translated :cct/whatever.
  [ccts-tags & _]
  (reduce (fn [m elem]
            (let [tag (:schema/tag elem)]
              (if-let [ccts-tag (oagis2ccts-key-map tag)]
                (if-let [content (:xml/content elem)]
                  (assoc m ccts-tag content)
                  m)
                (if (= tag :ROOT/CodeName)
                  (assoc m :iso/CodeName (:xml/content elem))
                  (do (log/warn "Unknown code-term tag" tag) m)))))
          {}
          ccts-tags))

;;; This one for inline-typedefs
(defmethod rewrite-xsd :inline-typedef
  [cplx-type & _]
  (assert (xml-type? cplx-type :xsd/complexType))
  (let [ccontent (xpath cplx-type :xsd/complexContent)
        ext-res (cond (xpath- ccontent :xsd/extension) :xsd/extension
                      (xpath- ccontent :xsd/restriction) :xsd/restriction)]
    ;(when (= "DescriptionTextType" (-> cplx-type :xml/attrs :name))
    ;  (reset! diag {:here 1 :elem cplx-type}))
    (as-> {} ?x
      (assoc ?x :schema/inline-component-name (-> cplx-type :xml/attrs :name)), 
      (let [dstring (:xml/content (xpath cplx-type :xsd/annotation :xsd/documentation))]
        (cond (string? dstring) (assoc ?x :schema/doc-string dstring)
              (vector? dstring) (assoc ?x :schema/ccts-data (rewrite-xsd dstring :oagis-ccts-def))
              :else ?x))
      (if-let [attrs (not-empty (filter #(xml-type? % :xsd/attribute) cplx-type))]
        (assoc ?x :schema/type-attrs (mapv #(rewrite-xsd % :oagis-type-attr) attrs))
        ?x)
      (if-let [elems (not-empty (filter #(xml-type? % :xsd/element)
                                        (:xml/content (xpath- cplx-type :xsd/sequence))))]
        (assoc ?x :schema/message-sequence (mapv #(rewrite-xsd % :oagis-message-elem) elems))
        ?x)
      (if-let [elems (not-empty (filter #(xml-type? % :xsd/element)
                                        (:xml/content (xpath- ccontent ext-res :xsd/sequence))))]
        (assoc ?x :schema/message-sequence  (mapv #(rewrite-xsd % :oagis-message-elem) elems))
        ?x)
      (if ext-res (assoc ?x ext-res (rewrite-xsd (xpath ccontent ext-res) ext-res)) ?x))))

(defmethod rewrite-xsd :oagis-include
  [xmap & _]
  (assert (xml-type? xmap :xsd/include))
  ;; The values below will be replaced by the schema names using :schema/pathname. 
  (str oagis-root (-> xmap :xml/attrs :schemaLocation (subs 3)))) ; POD It is NOT ../ from root. 
    
;;; Might want to check that the :schema/message-sequence resolve to something found
;;; either in :schema/inline-typedefs, or elsewhere.
;;; POD Fix :schema/pod-temp-include; I think I want something like :schema/imported-schemas
;;;     Will probably need to be cleaned up after reading all the schemas. 
(defmethod rewrite-xsd :oagis-message-schema
  [xmap & _]
  (let [content (-> (xpath xmap :xsd/schema) :xml/content)]
    (as-> xmap ?x
      (if-let [imports (not-empty (filter #(xml-type? % :xsd/include)
                                          (-> (xpath xmap :xsd/schema) :xml/content)))]
        (assoc ?x :mm/pod-temp-include (mapv #(rewrite-xsd % :oagis-include) imports)) ; <========== POD 
        ?x)
        (assoc ?x :schema/message-sequence
               (->> content
                    (filter #(xml-type? % :xsd/element))
                    (mapv #(rewrite-xsd % :oagis-message-elem))))
        (assoc ?x :schema/inline-typedefs 
               (->> content
                    (filter #(xml-type? % :xsd/complexType))
                    (mapv #(rewrite-xsd % :inline-typedef))))
        (dissoc ?x :xml/content :xml/ns-info))))

(defmethod rewrite-xsd :code-term
  ;; Return a map of the CCTS and ISO properties defined by an OAGIS or OAGIS/ISO code term.
  [xmap & _]
  (assert (xml-type? xmap :xsd/enumeration))
  (let [term (-> xmap :xml/attrs :value)
        doc    (xpath xmap :xsd/annotation :xsd/documentation)]
    {term (rewrite-xsd (:xml/content doc) :oagis-ccts-def)}))

(defmethod rewrite-xsd :code-list
  ;; Walk through a code list collecting terms.
  [xmap & _]
  (assert (xml-type? xmap :xsd/simpleType))
  (-> {}
      (assoc :schema/code-list-name (-> xmap :xml/attrs :name))
      (assoc :xsd/restriction   (rewrite-xsd (xpath xmap :xsd/restriction) :xsd/restriction))
      (assoc :schema/codes          (reduce (fn [m v]
                                          (merge m (rewrite-xsd v :code-term)))
                                        {}
                                        (filter #(xml-type? % :xsd/enumeration)
                                                (:xml/content (xpath xmap :xsd/restriction)))))))

(defmethod rewrite-xsd :code-list-schema
  [xmap & _]
  (let [content (-> (xpath xmap :xsd/schema) :xml/content)]
    (as-> xmap ?x
      (if (= (:schema/name ?x) "urn:oagis-10.6:CodeLists_1.xsd") ; POD Special case NYI
        (do
          (log/warn "Skipping CodeLists_1.xsd")
          (assoc ?x :mm/debug "Skipping"))
        (assoc ?x :schema/code-lists (mapv #(rewrite-xsd % :code-list)
                                       (filter #(xml-type? % :xsd/simpleType) content))))
      (dissoc ?x :xml/ns-info :xml/content))))

;;; POD look for more places that this can be used. 
(defmethod rewrite-xsd :xsd/simpleType
  ;; Translate an XSD simpleType. This is used at least for ISO 20022 schema, but maybe more. 
  [xmap & _]
  (let [restrict (xpath xmap :xsd/restriction)
        result (-> {}
                   (assoc :schema/type-name   (->  xmap :xml/attrs :name))
                   (assoc :schema/restriction (rewrite-xsd restrict :xsd/restriction)))]
    (cond (xpath xmap :xsd/restriction :xsd/enumeration) ; It is an enum-type restriction. 
          (assoc result :schema/enum-vals (mapv #(-> % :xml/attrs :value) (:xml/content restrict)))
          (every? xsd-constraint? (map :schema/tag (:xml/content restrict)))
          (if (-> restrict :xml/content not-empty)
            (assoc result :schema/type-attrs (reduce (fn [m elem]
                                                   (assoc m (:schema/tag elem)
                                                          (read-xsd-type
                                                           (:schema/tag elem)
                                                           (-> elem :xml/attrs :value))))
                                                 {}
                                                 (:xml/content restrict)))
            result)
          :else
          (do (log/warn "Unknown xsd constraint:"
                        (some #(not (xsd-constraint? %))
                              (map :schema/tag (:xml/content restrict))))
              (reset! diag xmap)))))

(defmethod rewrite-xsd :xsd/complexType
  ;; Translate a simple type. This is used at least for ISO 20022 schema, but maybe more. 
  [xmap & _]
  (assert (xml-type? xmap :xsd/complexType))
  (-> {}
      (assoc :schema/type-name (->  xmap :xml/attrs :name))
      (assoc :schema/message-sequence (mapv #(rewrite-xsd %) ; POD was :typical-elem
                                (filter #(or (xml-type? % :xsd/element)
                                             (xml-type? % :xsd/choice))
                                        (:xml/content (xpath xmap :xsd/sequence)))))))

(defmethod rewrite-xsd :iso-20022-schema
  ;; Translate an ISO 20022 schema, financial codes as simple and complex types. 
  [xmap & _]
  (let [content (-> (xpath xmap :xsd/schema) :xml/content)]
    (-> xmap
        (assoc :schema/simple-types
               (mapv #(rewrite-xsd % :xsd/simpleType)
                     (filter #(xml-type? % :xsd/simpleType) content)))
        (assoc :schema/complex-types
               (mapv #(rewrite-xsd % :xsd/complexType)
                     (filter #(xml-type? % :xsd/complexType) content)))
        (dissoc :xml/content :xml/ns-info))))
  
;;;=========================== Schema Operations ===========================================
(defn list-schemas
  "Return a list of schema, by default they are sorted by 'topic'"
  [& {:keys [sdo sort?] :or {sort? true}}]
  (let [base-names
        (if sdo
          (d/q `[:find [?n ...] :where [?s :schema/name ?n] [?s :schema/sdo ~sdo]] @conn)
          (d/q '[:find [?n ...] :where [_ :schema/name ?n]] @conn))]
    (if sort?
      (let [urns&topics (map (fn [urn topic] {:urn urn :topic topic})
                             base-names
                             (map q-schema-topic base-names))]
        (->> urns&topics
             (sort-by :topic)
             (mapv :urn)))
      (vec base-names))))

;;; So far, this resolves to
;;; ["... Model/Platform/2_6/Common/Components/Components.xsd"  (this one includes Meta.xsd)
;;;  "... Model/Platform/2_6/Common/Components/Meta.xsd"]       (this one incudes Extensions.xsd)
;;;  "... Extensions.xsd, Fields.xsd,
;;;  BusinessDataTypes_1.xsd ... which includes:
;;;     - All the CodeLists schemas
;;;     - IdentifierScheme_AgencyIdentification_3055_D16B.xsd
;;;     - XMLSchemaBuiltinType_1.xsd
(defn list-oagis-imports
  "Find all the schema imported by OAGIS schemas."
  []
  (d/q '[:find [?i ...]
         :where
         [_ :schema/sdo :oagi]
         [_ :schema/imported-schemas ?i]] @conn))

(defn get-term ; POD Does this work for OAGIS? 
  "Return a map describing what is known about the argument data type.
    - Schema-urn is a string
    - dt-id is a string naming a schema object such as a data type (e.g. 'AmountType')"
  [schema-urn term]
  (when-let [ent (d/q `[:find ?content .
                        :where 
                        [?schema :schema/name ~schema-urn]
                        [?schema :xml/content ?content] ; POD should this be :library/content
                        [?content :schema/term ~term]] ; a string like "AmountType"
                      @conn)]
    (du/resolve-db-id (dp/pull @conn '[*] ent) conn true)))

(defn get-term-schemas
  "Return all the schema containing the term (currently schema/term)"
  [term]
  (when-let [ents (not-empty (d/q `[:find [?ent ...]
                                    :where [?ent :schema/term ~term]]
                                  @conn))]
    (mapv (fn [ent] (d/q `[:find ?s .
                           :where
                           [?ent :xml/content ~ent] ; POD should this be :library/content
                           [?ent :schema/name ?s]]
                         @conn))
          ents)))

(defn get-schema
  "Return the map stored in the database for the given schema-urn."
  [schema-urn & {:keys [resolve?] :or {resolve? true}}]
  (when-let [ent  (d/q `[:find ?ent .
                         :where [?ent :schema/name ~schema-urn]] @conn)]
    (cond-> (dp/pull @conn '[*] ent)
      resolve? (du/resolve-db-id conn true))))

(defn add-schema-file!
  [path]
  (let [db-content (-> path read-clean rewrite-xsd util/condition-form vector)]
    (try
      (if (util/storable? db-content)
        (try (d/transact conn db-content) ; Use d/transact here, not transact! which uses a future.
             (catch Exception e
               (swap! bad-file-on-rebuild? conj path)
               (log/error "Error adding" path ":" e)))
        (do (swap! bad-file-on-rebuild? conj path)
            (log/error "Schema-map contains nils and cannot be stored." path)))
      (catch Exception e
        (swap! bad-file-on-rebuild? conj path)
        (log/error "Error checking storable?" path ":" e)))))

(defn add-schema-files!
  "Read a directory of files into the database.
   They consist of a rewritten (see rewrite-xsd) maps with some useful metadata. 
    DIR is the directory to read from. All the .xsd files there will be added to the DB."
  [dir]
  (let [grammar-matcher (.getPathMatcher 
                          (java.nio.file.FileSystems/getDefault)
                          "glob:*.xsd")
        files (->> (file-seq (io/file dir))
                   (filter #(.isFile %))
                   (filter #(.matches grammar-matcher (.getFileName (.toPath %))))
                   (mapv #(.getAbsolutePath %)))]
    (doseq [file files]
      (add-schema-file! file))))

(defn update-bad-files! 
  "On rebuild, note what couldn't be read."
  []
  (when (:rebuild-db? db-cfg)
    (doseq [file @bad-file-on-rebuild?]
      (d/transact conn [{:schema/pathname file
                         :mm/file-not-read? true}]))))

(defn bad-files
  "This creates a 'to do' list for debugging!"
  []
  (d/q '[:find [?p ...] :where
         [?ent :mm/file-not-read? true]
         [?ent :schema/pathname ?p]]
       @conn))

(defn unused-attrs 
  "Return a list of unused database attributes (for debugging)."
  []
  (let [unused (atom [])]
    (doseq [attr (map :db/ident db-schema)]
      (when (empty? (d/q `[:find ?e :where [?e ~attr ?]] @conn))
        (swap! unused conj attr)))
    @unused))

(defn add-topics! []
  (let [forms (reduce (fn [forms urn]
                        (if-let [topic (schema-topic urn)]
                          (conj forms {:schema/name urn :schema/topic topic})
                          forms))
                      []
                      (list-schemas))]
    (d/transact conn forms)))

(defn postprocess-schemas!
  "Do some additional work on schema already in the DB."
  []
  (add-topics!)
  (update-bad-files!))

;;;================================ Communication with Clients =========================================
;;; I think the key idea here for pathom-mediated composabiltiy is for each resolver to rename all db/id 
;;; to context specific names. These are currently #{:sdb/schema-id :sdb/elem-id :sdb/imported-schema-id}.
;;; (The last one isn't just a schema but a prefix too.)
;;; The simplest composition then is implemented as a flat table with values and foreign key references.
;;; I think, however, you can 'go deep' in the ::pc/output and still maintain. See all-schema-ids-r.  
;;; See also person-resolver at Part 6, 43:26. 

;;; For debugging
(def tname "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2")
(defn tryme [eql] ((var-get (resolve 'app.server.pathom/parser)) {} eql))
(def query-1229 [{[:sdb/schema-id 1229]
                  [:sdb/schema-id
                   {:schema/message-sequence [:schema-part/name
                                              :schema-part/type
                                              :schema-part/doc-string
                                              :schema-part/min-occurs
                                              :schema-part/max-occurs]}]}])

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

;;; Try it with (currently!):
;;;           [:sdb/elem-id 1305] (UBL "Invoice" element)
;;;           [:sdb/elem-id 5230] (OAGIS "Invoice" element)
(defn substructure-resolver
  "Return substructure for the argument elem-id"
  [elem-id]
  (if-let [owning-schema (d/q `[:find ?s :where [?s :schema/message-sequence ~elem-id]] @conn)]
    (let [ref-1 (d/q `[:find [?rs ...] :where [~owning-schema :schema/imported-schemas ?rs]] @conn)
          ref-2 (d/q `[:find [?rs ...] :where [~owning-schema :schema/inline-typedefs  ?rs]] @conn)
          refs (into ref-1 ref-2)
          elem-info (dp/pull @conn '[*] elem-id)]
      (if-let [ref-schema nil #_(schema-containing-ref)]
        :found-it
        (log/warn "Could not find referenced schema for" elem-id)))
    (log/warn "Could not find owning schema for" elem-id)))    
  

(def resolvers [schema-by-name-r
                schema-props-r
                elem-props-r
                list-r
                message-schema-r
                current-system-time-r])

;;;================================ Starting and Stopping ===========================================
;;; (user/restart) whenever you update the DB or the resolvers. (tools/refresh) if compilation fails.

(defn create-db!
  "Create the database if :rebuild? is true, otherwise just set the connection atom, conn."
  []
  (alter-var-root (var conn) (fn [_] (d/connect db-cfg)))
  (when (:rebuild-db? db-cfg)
    (reset! bad-file-on-rebuild? #{})
    (d/delete-database db-cfg)
    (d/create-database db-cfg)
    (d/transact conn db-schema)
    (add-schema-files! (str ubl-root "maindoc"))
    (add-schema-files! (str ubl-root "common"))
    (add-schema-files! (str oagis-root "Nouns"))
    (add-schema-files! (str oagis-root "Platform/2_6/Common"))
    (postprocess-schemas!))
  (log/info "Created schema DB"))

(defstate schema-db
  :start
  (do (create-db!) db-cfg))

(defn tryme2 []
  (let [path (str oagis-root "Platform/2_6/Common/Components/Components.xsd")]
    (-> path read-clean rewrite-xsd util/condition-form)))
