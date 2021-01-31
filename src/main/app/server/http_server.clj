(ns app.server.http-server
  (:require
   [clojure.pprint :refer [pprint]]
   [app.model.db]
   [app.server.config :refer [config]]
   [app.server.middleware :refer [middleware]]
   [mount.core :refer [defstate]]
   [org.httpkit.server :as http-kit]
   [taoensso.timbre :as log]))

(defstate http-server
  :start
  (let [cfg (::http-kit/config config)]
    (log/info "Starting HTTP Server with config " (with-out-str (pprint cfg)))
    (http-kit/run-server middleware cfg))
  :stop (http-server))
