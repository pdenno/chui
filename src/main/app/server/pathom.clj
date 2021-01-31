(ns app.server.pathom
  (:require
   [app.model.db :as db]
   [app.server.config :refer [config]]
   [clojure.core.async :as async :refer [<!!]]
   [com.wsscode.pathom.connect :as pc :refer [defresolver]]
   [com.wsscode.pathom.core :as p]
   [mount.core :refer [defstate]]
   [taoensso.timbre :as log]
   #_[com.wsscode.common.async-clj :refer [let-chan]]))

;;; Some of this from Chris O'Donnell's excellent blogging.
;;; See https://chrisodonnell.dev/posts/giftlist/initial_backend/ 
(defresolver index-explorer [env _]
  {::pc/input  #{:com.wsscode.pathom.viz.index-explorer/id}
   ::pc/output [:com.wsscode.pathom.viz.index-explorer/index]}
  {:com.wsscode.pathom.viz.index-explorer/index
   (-> (get env ::pc/indexes)
     (update ::pc/index-resolvers
       #(into {} (map (fn [[k v]] [k (dissoc v ::pc/resolve)])) %))
     (update ::pc/index-mutations
       #(into {} (map (fn [[k v]] [k (dissoc v ::pc/mutate)])) %)))})

(def all-resolvers [index-explorer db/resolvers])

(defn preprocess-parser-plugin
  "Helper to create a plugin that can view/modify the env/tx of a
  top-level request.
  f - (fn [{:keys [env tx]}] {:env new-env :tx new-tx})
  If the function returns no env or tx, then the parser will not be
  called (aborts the parse)"
  [f]
  {::p/wrap-parser
   (fn transform-parser-out-plugin-external [parser]
     (fn transform-parser-out-plugin-internal [env tx]
       (let [{:keys [env tx]} (f {:env env :tx tx})]
         (if (and (map? env) (seq tx))
           (parser env tx)
           {}))))})

(defn log-requests [{:keys [_ tx] :as req}]
  (log/debug "Pathom transaction:" (pr-str tx))
  req)

(defstate parser
  :start
  (let [real-parser
        (p/parallel-parser
          {::p/mutate  pc/mutate-async
           ::p/env     {::p/reader               [p/map-reader
                                                  pc/parallel-reader
                                                  pc/open-ident-reader
                                                  p/env-placeholder-reader]
                        ::p/placeholder-prefixes #{">"}}
           ::p/plugins [(pc/connect-plugin {::pc/register all-resolvers})
                        (p/env-wrap-plugin
                          (fn [env]
                            ;; Here is where you can dynamically add
                            ;; things to the resolver/mutation
                            ;; environment, like the server config,
                            ;; database connections, etc.
                            env))
                        (preprocess-parser-plugin log-requests)
                        p/error-handler-plugin
                        p/request-cache-plugin
                        (p/post-process-parser-plugin p/elide-not-found)
                        p/trace-plugin]})
        ;; NOTE: Add -Dtrace to the server JVM to enable Fulcro
        ;; Inspect query performance traces to the network tab.
        ;; Understand that this makes the network responses much
        ;; larger and should not be used in production.
        trace? (some? (System/getProperty "trace"))]
    (fn wrapped-parser [env tx]
      (<!! (real-parser env (if trace?
                              (conj tx :com.wsscode.pathom/trace)
                              tx))))))
