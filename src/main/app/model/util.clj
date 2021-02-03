(ns app.model.util
  (:require
   [cemerick.url                 :as url]
   [clojure.java.io              :as io]
   [clojure.string               :as str]))

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



