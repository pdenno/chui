(ns app.client
  (:require
   ;;[app.application :refer [APP]] ; POD Should this be APP ? No. Don't worry about it quite yet.
   ["react-codemirror2" :rename {UnControlled CodeMirror}]
   [com.fulcrologic.fulcro.application :as app]
   [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
   [com.fulcrologic.fulcro.routing.legacy-ui-routers :as r :refer [defsc-router]]
   [com.fulcrologic.fulcro.data-fetch :as df]
   [com.fulcrologic.fulcro.dom :as dom :refer [div]]
   [com.fulcrologic.fulcro.networking.http-remote :as http]
   [com.fulcrologic.fulcro.algorithms.react-interop :as interop]
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

(def ui-code-mirror (interop/react-factory CodeMirror))
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
(fm/defmutation run-test-modal [{:keys [some-text]}]
  (action [{:keys [state]}] ; This is typically what you want to do locally. Binding to the state db.
          (log/info "I would run a function here? some-text=" some-text)
          #_(-> js/document (.getElementById "TenQuestions"))))

(def std-start
  {:a1 #:square{:player :white :piece :rook}
   :b1 #:square{:player :white :piece :knight}
   :c1 #:square{:player :white :piece :bishop}
   :d1 #:square{:player :white :piece :queen}
   :e1 #:square{:player :white :piece :king}
   :f1 #:square{:player :white :piece :bishop}
   :g1 #:square{:player :white :piece :knight}
   :h1 #:square{:player :white :piece :rook}
   :a2 #:square{:player :white :piece :pawn}
   :b2 #:square{:player :white :piece :pawn}
   :c2 #:square{:player :white :piece :pawn}
   :d2 #:square{:player :white :piece :pawn}
   :e2 #:square{:player :white :piece :pawn}
   :f2 #:square{:player :white :piece :pawn}
   :g2 #:square{:player :white :piece :pawn}
   :h2 #:square{:player :white :piece :pawn}

   :a8 #:square{:player :black :piece :rook}
   :b8 #:square{:player :black :piece :knight}
   :c8 #:square{:player :black :piece :bishop}
   :d8 #:square{:player :black :piece :queen}
   :e8 #:square{:player :black :piece :king}
   :f8 #:square{:player :black :piece :bishop}
   :g8 #:square{:player :black :piece :knight}
   :h8 #:square{:player :black :piece :rook}
   :a7 #:square{:player :black :piece :pawn}
   :b7 #:square{:player :black :piece :pawn}
   :c7 #:square{:player :black :piece :pawn}
   :d7 #:square{:player :black :piece :pawn}
   :e7 #:square{:player :black :piece :pawn}
   :f7 #:square{:player :black :piece :pawn}
   :g7 #:square{:player :black :piece :pawn}
   :h7 #:square{:player :black :piece :pawn}})

(def board-squares
  (vec (for [irank (map str (range 1 9))
             ifile ["a" "b" "c" "d" "e" "f" "g" "h"]]
         (keyword (str ifile irank)))))

(defn square-coords
  "Return a vector of two integers [file rank] given an id string."
  [id]
  (let [file2num {\a 1 \b 2 \c 3 \d 4 \e 5 \f 6 \g 7 \h 8}]
    (vector
     (-> id name (get 0) file2num)
     (-> id name (get 1) js/parseInt))))

(defsc Square [this {:square/keys [id player piece] :as props}]
  {:query [:square/id :square/player :square/piece]
   :ident :square/id
   :initial-state (fn [p] ; can't use props (above) here. 
                    (let [{:square/keys [player piece]} (std-start (:square/id p))]
                      (cond-> {:square/id (:square/id p)}
                        player (assoc :square/player player)
                        piece  (assoc :square/piece  piece))))}
  (let [[x y] (square-coords id)
        white "#f5e9dc"
        black "#8f7f7f"]
    (dom/td
     {:style {:padding "0"}}
     (dom/button {:style  {:border "0" :width "64" :height "64"
                           :background (if (odd? (+ x y)) black white)}}
                 #_(dom/img {:src "knight-with-white.svg" :height "50" :width "50"})))))

(def ui-square (comp/factory Square {:keyfn :square/id}))

(defsc Board [this props #_{:board/keys [id board-state] :as props}]
  {:query [:board/id {:board/board-state (comp/get-query Square)}]
   :ident :board/id
   :initial-state
   (fn [_] {:board/board-state (reduce (fn [m id] (assoc m id (comp/get-initial-state Square {:square/id id})))
                                       {}
                                       board-squares)})}
    (div :.ui.celled.table
         (dom/tbody 
             (for [irank (map str (range 1 9))]
               (dom/tr
                (for [ifile ["a" "b" "c" "d" "e" "f" "g" "h"]]
                  (ui-square {:square/id (keyword (str ifile irank))})))))))
                  
(def ui-board (comp/factory Board))

;;; (. (. js/document -body) -clientHeight)
;;; (. (. js/document -body) -clientWidth)

(defsc Root [_ props]
  {:query [:root/game-moves :board/board-state]
   :initial-state (fn [_] {:board/board-state (:board/board-state (comp/get-initial-state Board {:board/id ::board}))
                           :root/game-moves []})}
  (div :.ui.container
       (dom/h1 "Chui")
       (ui-board {:board/id ::board})))

;;; POD, this reloads initial-state on first call. Subsequent calls don't change the DB.
;;; Hypothesis about how the df/load! works:
;;;  SourceSchema (likewise TargetSchema) is identified (:ident) by :list/id, so that a query can be made:
;;;  [{:message-schema [:list/id {:list/schemas [:sdb/schema-id :schema/name]}]}]
;;;  The response from this query supplies the other argument, list/schema as evident from the query. 
(defn ^:export init []
  (app/mount! APP Root "app")
  (df/load! APP :server/time nil {:target [:root/game-start-time]})
  (js/console.log "Loaded"))

;;; POD This defines what is to be done when the file is saved.
(defn ^:export refresh []
  ;; re-mounting will cause forced UI refresh
  (app/mount! APP Root "app")
  ;; 3.3.0+ Make sure dynamic queries are refreshed
  (js/console.log "Hot reload"))
