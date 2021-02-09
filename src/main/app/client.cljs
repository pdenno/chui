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
   [goog.object :as gobj]))

;;; To remember:
;;; (0) ??? The value of a destructured props can be a map defined by sc's the query. Or do I have it backwards?
;;;     That is, does the query set up a binding of variables to maps representing nodes in the tree. 
;;; (1) It is an error to destructure something in props and not use it in the query. This is because
;;;     the purpose of the destructuring is to pick out what in the query path is relevant to the sc.
;;; (2) It is likewise an error for the :ident not to appear in the query.
;;;     This is because it is otherwise impossible to say what part of the application's data the sc concerns.
;;; (3) :query describes the shape of the data managed. It is NOT CLEAR whether it is necessary that some of
;;;     that data has to be tied to the ::app/state-atom tree. In the recursive person example
;;;     (see Section 6.9 Recursive Queries) only the root person is in ::app/state-atom. The rest is in :initial-state
;;;     and never added to the atom. 
;;; (4) Properties are always passed to a component factory as the first argument and are not optional"
;;;     (exception in Chapter 17). Also my own exception: menu-source
;;; (5) :initial-state sets state at the relative root (root of tree at the ident).
;;; (6) When you do df/load! like this:
;;;    (df/load! APP [:sdb/schema-id 1229] SourceStructure)
;;;    It creates a pathom transaction like this (because it is uses information from the sc).
;;;  [{[:sdb/schema-id 1229]
;;;      [:sdb/elem-id
;;;       :schema-part/name
;;;       :schema-part/min-occurs
;;;       :schema-part/max-occurs]}}]}]
;;; (7) Every sc that should appear initially should have an :initial-state; compose these towards root.
;;; (8) :ident, :query, and :initial state can all be specified as a function or template form.
;;;     The template form has the advantage of sanity checking. The functional form of :ident can use
;;;     (close over) 'this' and props from the defsc argument list.
;;; (9) For :ident, a literal keyword is an abreviation. For example :sdb/schema-id is an abreviation of
;;;     [:sdb/schema-id :sdb/schema-id] where the first element is a literal and the second is the name
;;;     of the property to pul from props.
;;; (10) Don't make deep Pathom queries; normalize.
;;; (11) Don't specify :initial-state for thing you are going to df/load!
;;; (12) As it says in the minimalist guide:
;;;      Don’t be mislead, the query is not a standalone query that could be "run" directly against the database
;;;      (as you know from SQL or re-frame subscriptions). It is rather a query fragment, which only makes sense in
;;;      the context of its parent’s query. Only the root componet’s properties are resolved directly against the
;;;      client database or, when load!-ed, against global Pathom resolvers. A query such as [:person/id :person/fname] is
;;;      meaningless on its own - which person? Only in the context of a parent, such
;;;      as [{:all-people [<insert here>]}] (in an imaginary AllPeopleList component) does it make sense.

(def diag (atom nil))

(defonce APP (app/fulcro-app
              {:remotes {:remote (http/fulcro-http-remote {})}}))

;;; Sometimes it is best to just look at a JavaScript example!
;;; https://dev.to/caicindy87/form-in-a-semantic-ui-react-modal-n5k
(defsc GoalModal [this {:ui/keys [modal-id]}]
  {:query [:ui/modal-id]
   :ident (fn [] [:ui/modal-id (:ui/keys modal-id)])} ; <============== DOESN'T SEEM TO BE WHAT I'D WANT!
  (div (dom/button :.ui.button.yellow.create_btn 
                   {:onClick (fn []
                               (when-let [m #_(gobj/get this modal-id) (-> js/document (.getElementById modal-id))]
                                 (log/info "modal = " m)
                                 (.modal m "show")))} ; See https://semantic-ui.com/modules/modal.html#/usage
                   "Test Modal")
       (div :.ui.modal {:id modal-id} 
            (div :.description
                 (div :.ui.header "I am wondering how this works...")
                 (dom/p "...and I'm trying a few things."))
            (div :.actions
                 (div :.ui.black.deny.button "Nope")
                 (div :.ui.positve.right.labeled.icon.button "Yup")))))

(def ui-goal-modal (comp/factory GoalModal))

(fm/defmutation run-test-modal [{:keys [some-text]}]
  (action [{:keys [state]}] ; This is typically what you want to do locally. Binding to the state db.
          (log/info "I would run a function here? some-text=" some-text)
          #_(-> js/document (.getElementById "TenQuestions"))))

;;; (aget (-> js/document (.getElementById "TenQuestions")) "click")

;;; NOTE: $ is an alias for jQuery(). $("#test") gets the test div.
;;; In jQuery, the class and ID selectors are the same as in CSS.
;;; If you want to select elements with a certain class, use a dot ( . ) and the class name.
;;; If you want to select elements with a certain ID, use the hash symbol ( # ) and the ID name
;;; Example JS for running testmodal 
;;;$(function(){
;;;	$("#test").click(function(){
;;;		$(".test").modal('show');
;;;	});
;;;	$(".test").modal({
;;;		closable: true
;;;	});
;;;});

;;;========================================== CHUI =============================================
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
                 (when (and (= player :black) (= piece :knight))
                   (dom/img {:src "knight-with-white.svg" :height "50" :width "50"}))))))

(def ui-square (comp/factory Square {:keyfn :square/id}))

;;; ?!?!? (comp/get-query Square) is NOT THE SAME as the literal below?!?!
;;; Is this because some of the :piece and :player values are :fulcro/not-found?
(defsc Board [_ props]
  {:query [[:board/id ::board] {:board/start [:square/id :square/player :square/piece]}]
   :ident (fn [] [:board/id ::board])}
  (let [start (get-in props [[:board/id ::board] :board/start])]
    (dom/table :.ui.celled-table                   
      (apply dom/tbody 
             (for [irank (range 8 0 -1)]
               (apply dom/tr {:id irank} 
                      (for [ifile ["a" "b" "c" "d" "e" "f" "g" "h"]]
                        (let [id (keyword (str ifile irank))]
                          (when-let [sqr (some #(when (= (:square/id %) id) %) start)]
                            (ui-square sqr))))))))))

(def ui-board (comp/factory Board))

;;; (. (. js/document -body) -clientHeight)
;;; (. (. js/document -body) -clientWidth)
;;; (comp/get-initial-state Root {}) ; Sometimes useful. Not so much here. 
(defsc Root [_ props]
  {:query [[:board/id ::board] {:board/start [:square/id :square/piece :square/player]}]
   :initial-state (fn [_] {:game/turn :white ; Don't put here things df/load!-ed.
                           :game/move 1
                           :game/history []})}
  (div :.ui.container
       (dom/h1 "Chui")
       (ui-board props)))

;;; POD, On the first call, this loads things that you want to be in the initial state.
;;; Subsequent calls don't change the DB.
(defn ^:export init []
  (app/mount! APP Root "app")
  ;; Don't specify :initial-state for anything you intend to df/load! from the server!
  ;; Use (e.g. Board) to normalize. Use target to put it on the root.
  (df/load! APP :server/time   nil {:target [:game/start-time]}) ; nil means don't normalize.
  (df/load! APP [:board/id ::board] Board)
  (js/console.log "Loaded"))

;;; POD This defines what is to be done when the file is saved.
(defn ^:export refresh []
  ;; re-mounting will cause forced UI refresh
  (app/mount! APP Root "app")
  ;; 3.3.0+ Make sure dynamic queries are refreshed
  (comp/refresh-dynamic-queries! APP)
  (js/console.log "Hot reload"))
