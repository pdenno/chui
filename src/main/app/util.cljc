(ns app.util
  #?(:cljs (:refer-clojure :exclude [uuid]))
  (:require [com.fulcrologic.guardrails.core :refer [>defn]]
            #?(:clj [cognitect.transit :as t])
            [clojure.spec.alpha :as s])
  #?(:clj (:import [java.time Instant])))

(>defn uuid
  "Generate a UUID the same way via clj/cljs.  Without args gives random UUID. With args, builds UUID based on input (which
  is useful in tests)."
  #?(:clj ([] [=> uuid?] (java.util.UUID/randomUUID)))
  #?(:clj ([int-or-str]
           [(s/or :i int? :s string?) => uuid?]
           (if (int? int-or-str)
             (java.util.UUID/fromString
               (format "ffffffff-ffff-ffff-ffff-%012d" int-or-str))
             (java.util.UUID/fromString int-or-str))))
  #?(:cljs ([] [=> uuid?] (random-uuid)))
  #?(:cljs ([& args]
            [(s/* any?) => uuid?]
            (cljs.core/uuid (apply str args)))))

(def transit-write-handlers
  #?(:clj
     {Instant (t/write-handler "m"
                (fn [^Instant t] (.toEpochMilli t))
                (fn [^Instant t] (.toString (.toEpochMilli t))))}
     :cljs {}))

(def transit-read-handlers
  #?(:clj
     {"m" (t/read-handler
            (fn [s] (Instant/ofEpochMilli s)))}
     :cljs {}))
