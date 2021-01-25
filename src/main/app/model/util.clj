(ns app.model.util
  (:require
   [cemerick.url                 :as url]
   [clojure.data.xml             :as x]
   [clojure.java.io              :as io]
   [clojure.string               :as str]
   [taoensso.timbre              :as log]
   [clojure.walk                 :as walk]))

(def diag (atom nil))

(defn keywordize
  "Return the string as a keyword. If the string as a colon in it,
  the prefix is used as the keyword's namespace."
  ([str]
   (if (string? str)
     (if-let [[_ prefix word] (re-matches #"(\w+)\:(\w+)" str)]
       (keyword prefix word)
       (keyword str))
     str))
  ([str ns]
    (keyword ns str)))

(def keywords?
  "A set of map keys corresponding to values that should be keywordized"
  #{:xsd/extension :cct/PrimitiveType :cct/sc-use #_:cc-db/sc-use #_:cc-db/sc-type :cct/sc-type :cct/CategoryCode})

(def needs-zip?
  "A set of map keys where I need to encode a map for later decoding (See db-utils/resolve-db-id)"
  #{:schema/codes})

(defn condition-form
  "Return the form with certain map values as keywords and certain map values zipped."
  [form & {:keys [key-pred] :or {key-pred keywords?}}]
  (cond (map? form) (reduce-kv (fn [m k v]
                                 (if (needs-zip? k)
                                   (-> m
                                       (assoc :mm/zip-keys (keys v))
                                       (assoc :mm/zip-vals (vals v)))
                                   (if (key-pred k)
                                     (assoc m k (keywordize v))
                                     (assoc m k (condition-form v)))))
                                 {}
                                 form)
        (vector? form)   (mapv condition-form form)
        (set? form) (set (map  condition-form form))
        (coll? form)     (map  condition-form form)
        :else form))

;;; POD ToDo: Spec about this?
(defn storable?
  "Return true if the argument contains no nils.
   Such data cannot be stored in datahike."
  [obj]
  (let [ok? (atom true)]
    (letfn [(storable-aux [obj]
              (cond (not @ok?) false
                    (nil? obj) (reset! ok? false)
                    (map? obj) (reset! ok? (reduce-kv (fn [result _ v] (cond (not @ok?) false
                                                                             (not result) false
                                                                             (nil? v) false
                                                                             :else (storable-aux v)))
                                                      true
                                                      obj))
                    (coll? obj) (reset! ok? (every? storable-aux obj))
                    :else true))]
      (storable-aux obj))
    @ok?))

(defn remove-nils
  "Return the object with nils removed. USE OF THIS DISCOURAGED."
  [obj]
  (let [pname (:schema/pathname obj)]
    (letfn [(remove-nils-aux [obj]
              (walk/prewalk
               (fn [obj]
                 (cond (map? obj) (reduce-kv (fn [m k v]
                                               (cond (nil? k) (do (log/info "nil key in map, value=" v "file=" pname) m)
                                                     (nil? v) (do (log/info "nil val in map, key=" k "file=" pname) m)
                                                     :else (assoc m k v)))
                                             {}
                                             obj)
                       (vector? obj) (mapv remove-nils-aux obj)
                       (set? obj)    (set (map remove-nils-aux obj))
                       (coll?   obj) (map remove-nils-aux obj)
                       (nil? obj)    (do (log/info "nil value replaced." "file=" pname) :mm/nil)
                       :else obj))
               obj))]
      (remove-nils-aux obj))))
           
(defn clean-whitespace
  "Remove whitespace in element :content."
  [xml]
  (walk/postwalk
   (fn [obj]
     (if (and (map? obj) (contains? obj :content))
       (if (= 1 (count (:content obj))) ;; POD Maybe don't remove it if is the only content???
         obj
         (update obj :content (fn [ct] (remove #(and (string? %) (re-matches #"^\s*$" %)) ct))))
       obj))
   xml))

(defn explicit-root-ns
  "Return argument x/element-nss map modified so that that the empty-string namespace is 'root' or whatever
   If the schema uses 'xs' for 'http://www.w3.org/2001/XMLSchema', change it to xsd"
  [nspaces & {:keys [root-name] :or {root-name "ROOT"}}]
  (when (-> nspaces :p->u (contains? root-name))
    (log/warn "XML uses explicit 'root' namespace alias.")) ; POD so pick something else. NYI.
  (as-> nspaces ?ns
    (assoc-in ?ns [:p->u root-name] (or (get (:p->u ?ns) "") :mm/nil))
    (update ?ns :p->u #(dissoc % ""))
    (update ?ns :u->ps
            (fn [uri2alias-map]
              (reduce-kv (fn [res uri aliases]
                           (assoc res uri (mapv #(if (= % "") root-name %) aliases)))
                         {}
                         uri2alias-map)))
    ;; Now change "xs" to "xsd" if it exists.
    (if (= "http://www.w3.org/2001/XMLSchema" (get (:p->u ?ns) "xs"))
      (as-> ?ns ?ns1
        (assoc-in ?ns1 [:p->u "xsd"] "http://www.w3.org/2001/XMLSchema")
        (update ?ns1 :p->u  #(dissoc % "xs"))
        (update ?ns1 :u->ps #(dissoc % "http://www.w3.org/2001/XMLSchema"))
        (assoc-in ?ns1 [:u->ps "http://www.w3.org/2001/XMLSchema"] ["xsd"]))
      ?ns)))

;;; POD Currently this isn't looking for redefined aliases. It calls x/element-nss just once!
;;; (-> sample-ubl-message io/reader x/parse alienate-xml)
(defn alienate-xml ; Silly, but I like it!
  "Replace namespaced xml map keywords with their aliases."
  [xml]
  (let [ns-info (-> xml x/element-nss explicit-root-ns)]
    (letfn [(equivalent-tag [tag]
              (let [[success? ns-name local-name] (->> tag str (re-matches #"^:xmlns\.(.*)/(.*)$"))]
                (if success?
                  (let [ns-name (url/url-decode ns-name)]
                    (if-let [alias-name (-> ns-info :u->ps (get ns-name) first)]
                      (keyword alias-name  local-name)
                      (keyword ns-name     local-name)))
                  tag)))]
      (walk/postwalk
       (fn [obj]
         (if (and (map? obj) (contains? obj :tag))
           (update obj :tag equivalent-tag)
           obj))
       xml))))

;;; (detagify '{:tag :cbc/InvoiceTypeCode, :attrs {:listID "UN/ECE 1001 Subset", :listAgencyID "6"}, :content ("380")})
(defn detagify
  "Argument in content from clojure.data.xml/parse. Return a map where
    (1) :tag is :schema/type,
    (2) :content, if present, is a simple value or recursively detagified. 
    (3) :attrs, if present, are :xml/attrs.
   The result is that
     (a) returns a string or a map that if it has :xml/content, it is a string or a vector.
     (b) if a map, and the argument had attrs, has an :xml/attrs key."
  [obj]
  (cond (map? obj)
        (as-> obj ?m
          (assoc ?m :schema/tag (:tag ?m))
          (if (not-empty (:attrs   ?m)) (assoc ?m :xml/attrs (:attrs ?m)) ?m)
          (if (not-empty (:content ?m)) (assoc ?m :xml/content (detagify (:content ?m))) ?m)
          (dissoc ?m :tag :attrs :content))
        (seq? obj) (if (and (== (count obj) 1) (-> obj first string?))
                     (first obj)
                     (mapv detagify obj))
        (string? obj) obj ; It looks like nothing will be number? Need schema to fix things. 
        :else (throw (ex-info "Unknown type in detagify" {:obj obj}))))

(defn read-xml
  "Return a map of the XML file read."
  [pathname]
  (let [xml (-> pathname io/reader x/parse)]
     {:xml/ns-info (explicit-root-ns (x/element-nss xml))
      :xml/content (-> xml alienate-xml clean-whitespace detagify vector)
      :schema/pathname pathname}))

;;; POD Could enhance this to be REAL XPath. 
(defn xpath
  "Content is a map with :xml/content. Follow the path, each step of
   which selects something from argument's :xml/content
   either an :schema/tag element, in which the first such is chosen, or an index,
   in which case that one is "
  [content & path-in]
  (loop [result content
         path path-in]
    (cond (empty? path) result,
          (not (map? content)) (log/warn "xpath failed at:" path "in" path-in),
          :else
          (let [search (first path)]
            (recur
             (if (number? search)
               (nth (:xml/content result) search)
               (some #(when (= search (:schema/tag %)) %) (:xml/content result)))
             (rest path))))))

;;; POD Fix this!
(defn xpath-
  "Content is a map with :xml/content. Follow the path, each step of
   which selects something from argument's :xml/content
   either an :schema/tag element, in which the first such is chosen, or an index,
   in which case that one is "
  [content & path-in]
  (loop [result content
         path path-in]
    (cond (empty? path) result,
          (not (map? content)) nil
          :else
          (let [search (first path)]
            (recur
             (if (number? search)
               (nth (:xml/content result) search)
               (some #(when (= search (:schema/tag %)) %) (:xml/content result)))
             (rest path))))))

             
;;; POD More sophisticated usage???  
(defn xml-type?
  "Return true if the content has :schema/tag = the argument."
  [xml xtype]
  (= (:schema/tag xml) xtype))

(defn nspaces
  "Return a string of n spaces."
  [n]
  (reduce (fn [s _] (str s " ")) "" (range n)))

