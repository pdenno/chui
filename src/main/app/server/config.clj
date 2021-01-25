(ns app.server.config
  (:require
    [mount.core :refer [defstate args]]
    [com.fulcrologic.fulcro.server.config :refer [load-config!]]
    [taoensso.timbre :as timbre]))

;;; POD Currently running this will cause logging to the console to be lost. 
(defn configure-logging! [config]
  #_(let [{:keys [::timbre/logging-config]} config]
    (timbre/info "Configuring Timbre with " logging-config)
    (timbre/merge-config! logging-config)))

(defstate config
  :start (let [{:keys [config] :or {config "config/dev.edn"}} (args)
               configuration (load-config! {:config-path config})]
           (timbre/info "Loaded config" config)
           (configure-logging! configuration)
           configuration))

