(ns app.server.middleware
  (:require
    [app.server.config :refer [config]]
    [app.server.pathom :refer [parser]] ; parser is mount/defstate in that file.
    [app.util :as util]
    [mount.core :refer [defstate]]
    [com.fulcrologic.fulcro.server.api-middleware :as fmid]
    [ring.middleware.defaults :refer [wrap-defaults]]
    [ring.middleware.gzip :refer [wrap-gzip]]
    [ring.middleware.resource :refer [wrap-resource]]
    [ring.util.response :as resp]))

;;; Some of this from Chris ODonnell's excellent blogging
;;; (See https://chrisodonnell.dev/posts/giftlist/initial_backend/)
(defn- not-found-handler [_]
  (assoc-in (resp/resource-response "public/index.html")
            [:headers "Content-Type"] "text/html"))

(defn- wrap-api [handler parser uri]
  (fn [request]
    (if (= uri (:uri request))
      {:status 200
       :body (parser {:ring/request request}
                     (:transit-params request))
       :headers {"Content-Type" "application/transit+json"}}
      (handler request))))

;;; The wrap-api middleware returns a response with the result of calling the parser for
;;; requests to the /api url, adding the ring request as the parser’s env.
;;; Commented code and alternatives from Chris's blog are owing to
;;;  clojure.lang.ArityException: Wrong number of args (1) passed to: app.server.pathom/eval109804/fn--109805/wrapped-parser--109808
;;;  You get that with e.g. a Fulcro Inspect EQL request.
;;;  Note the difference between server/pathom.clj and Part 6 4:34: Chris's parser takes TWO arguments 
(defstate middleware
  :start 
  (let [site-defaults (:ring.middleware/defaults-config config)]
    (-> #_fmid/not-found-handler ; Used in Part 6
        not-found-handler
        (wrap-api parser "/api")
        #_(fmid/wrap-api {:parser parser :uri "/api"}) ; Used in Part 6
        (fmid/wrap-transit-params   {:opts {:handlers util/transit-read-handlers}})
        (fmid/wrap-transit-response {:opts {:handlers util/transit-write-handlers}})
        (wrap-resource "public")
        (wrap-defaults (-> site-defaults
                           (assoc-in [:security :anti-forgery] false)
                           (assoc-in [:security :xss-protection :enable?] false)))
        wrap-gzip)))
