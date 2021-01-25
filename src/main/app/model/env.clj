(ns app.model.env
  (:require
   [mount.core         :refer [defstate args]]
   [clojure.java.io    :as io]
   [cprop.core         :refer [load-config]] 
   [cprop.source       :as source]
   [taoensso.timbre    :as log]))

(defn hi-props []
  (let [tmp-dir (str (System/getProperty "user.home") "/tmp/mapper/")]
    (io/make-parents (str tmp-dir "foo.txt"))
    {:tmp-dir tmp-dir}))

;;; env is the mount/state map tracking details of the running environment including
;;; environment variables, java-class-path and things from the env/{prod|dev|test/}/config.env file. 
(defstate env
  :start
  (do (log/info "Loading config")
      (load-config ; https://github.com/tolitius/cprop#setting-the-conf-system-property
       :merge
       [(args)
        (hi-props)
        (source/from-system-props)    ; java System stuff, including an amazingly long class path!
        (source/from-env)])))         ; real environment variables
