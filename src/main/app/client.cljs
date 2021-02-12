(ns app.client
  (:require
   [com.fulcrologic.fulcro.application :as app]
   [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
   [com.fulcrologic.fulcro.routing.legacy-ui-routers :as r :refer [defsc-router]]
   [com.fulcrologic.fulcro.data-fetch :as df]
   [com.fulcrologic.fulcro.dom :as dom :refer [div]]
   [com.fulcrologic.fulcro.networking.http-remote :as http]
   [com.fulcrologic.fulcro.algorithms.denormalize :as fdn] ; fdn/db->tree (see also fnorm/tree->db).
   [taoensso.timbre :as log]
   [com.fulcrologic.fulcro.algorithms.merge :as merge]
   [com.fulcrologic.fulcro.mutations :as fm #_:refer #_[defmutation]]
   [com.fulcrologic.fulcro.inspect.inspect-client :as inspect]
   [com.fulcrologic.semantic-ui.modules.modal.ui-modal :refer [ui-modal]]
   [goog.object :as gobj]))

(defonce APP (app/fulcro-app
              {:remotes {:remote (http/fulcro-http-remote {})}}))

(fm/defmutation dismiss-modal [_]
  (action [{:keys [state]}] ; This is typically what you want to do locally. Binding to the state db.
          (swap! state #(assoc % :ui/modal-visible? false))))

;;; I probably want this to something more, maybe a union. 
(defsc ModalContent [this props]
  {:query [:ui/modal-content-id]
   :ident (fn [] [:ui/modal-content-id ::modal-content])}
  (div :.ui.container
       (dom/p "This is my modal's content.")
       (dom/button :.ui.button.yellow.create_btn 
                   {:onClick #(comp/transact! this [(dismiss-modal {})])}
                   "Dismiss")))

(def ui-modal-content (comp/factory ModalContent))

(defn square-coords
  "Return a vector of two integers [rank file] given an id string."
  [id]
  (let [file2num {\a 1 \b 2 \c 3 \d 4 \e 5 \f 6 \g 7 \h 8}]
    (vector
     (-> id name (get 1) js/parseInt)
     (-> id name (get 0) file2num))))

(defsc Square [_ {:square/keys [id player piece]}]
  {:query [:square/id :square/player :square/piece]
   :ident :square/id}
  (let [[x y] (square-coords id)
        white "#f5e9dc"
        black "#8f7f7f"]
    (dom/td
     {:style {:padding "0"}}
     (dom/button {:style  {:border "0" :width "64" :height "64"
                           :background (if (odd? (+ x y)) white black)}}
                 (when (and (= player :black) (= piece :knight)) ; this just for testing
                   (dom/img {:src "knight-with-white.svg" :height "50" :width "50"}))))))

(def ui-square (comp/factory Square {:keyfn :square/id}))

(defsc Board [_ props]
  {:query [{:board/start (comp/get-query Square)}]
   :ident (fn [] [:board/id ::board])}
  (let [start (get-in props [[:board/id ::board] :board/start])]
    (dom/table :.ui.celled-table                   
      (dom/tbody 
       (for [irank (range 8 0 -1)]
         (dom/tr {:key irank} 
                 (for [ifile ["a" "b" "c" "d" "e" "f" "g" "h"]]
                   (let [id (keyword (str ifile irank))]
                     (when-let [sqr (some #(when (= (:square/id %) id) %) start)]
                       (ui-square sqr))))))))))

(def ui-board (comp/factory Board))

(defsc ModalWrapper [_ {:ui/keys [modal-visible?]}]
  {:query [:ui/modal-visible?]
   :ident (fn [] [:ui/modal-wrapper-id ::modal-wrapper])}
  ;; For children might use a union, might just use more props. 
  (ui-modal {:children (ui-modal-content {:ui/modal-content-id ::modal-content})
             :open modal-visible?}))

(def ui-modal-wrapper (comp/factory ModalWrapper))

(def diag (atom nil))

;;; (. (. js/document -body) -clientHeight)
;;; (. (. js/document -body) -clientWidth)
;;; (comp/get-initial-state Root {}) ; Sometimes useful. Not so much here.
;;; props are built from the query. They will be sent to ui-board, 
;;; which is why they include the :square/id table. That info is needed.
(defsc Root [_ props]
  {:query [{[:board/id ::board] (comp/get-query Board)}
           {:square/id (comp/get-query Square)}
           {:ui/modal-visible? (comp/get-query ModalWrapper)}]
   :initial-state (fn [_] {:ui/modal-visible? true ; POD Of course, I don't want this permanently!
                           :game/turn :white ; Don't put here things df/load!-ed.
                           :game/move 1
                           :game/history []})}
  (div :.ui.container
       (dom/h1 "Chui")
       (ui-modal-wrapper {:ui/modal-wrapper-id ::modal-wrapper
                          :ui/modal-visible? (:ui/modal-visible? props)})
       (ui-board props)))

;;; POD, On the first call, this loads things that you want to be in the initial state.
;;; Subsequent calls don't change the DB.
(defn ^:export init []
  (app/mount! APP Root "app")
  ;; Don't specify :initial-state for anything you intend to df/load! from the server!
  ;; Use (e.g. Board) to normalize. Use target to put it on the root.
  (df/load! APP :server/time nil {:target [:game/start-time]}) ; nil means don't normalize.
  (df/load! APP [:board/id ::board] Board)
  (js/console.log "Loaded"))

;;; POD This defines what is to be done when the file is saved.
(defn ^:export refresh []
  ;; re-mounting will cause forced UI refresh
  (app/mount! APP Root "app")
  ;; 3.3.0+ Make sure dynamic queries are refreshed
  (comp/refresh-dynamic-queries! APP)
  (js/console.log "Hot reload"))
