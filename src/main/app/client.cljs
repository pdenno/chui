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

;;; Note how this uses the backpointer to :sdb/elem-id !
(defsc SchemaPart [_this {:sdb/keys [elem-id] :schema-part/keys [name min-occurs max-occurs] :as props}]
  {:query [:sdb/elem-id :schema-part/name :schema-part/type :schema-part/min-occurs :schema-part/max-occurs]
   :ident :sdb/elem-id #_(fn [] [:sdb/elem-id (:sdb/elem-id props)])} ;<================= SIMPLIFY ????
  (div {:id elem-id} (str elem-id name min-occurs max-occurs)))

(def ui-schema-part (comp/factory SchemaPart {:keyfn :sdb/elem-id})) ; Everything that you map over should have a :keyfn.

(defn elem-path [id] [:sdb/elem-id id])
(defn add-elem [state-map id elem] (assoc-in state-map (elem-path id) elem))

;;; Calling a fm/defmutation returns a list that looks like a function call.
;;; (add-schema-property {:schema/name "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2"})
;;; ==> (app.client/add-schema-property {:schema/name "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2"})
;;; It also registers this as a multi-method. To really make use of it, you call it with comp/transact!
;;; transact! submits from the UI an abstract operation that may have local and/or remote effects to your application.
;;; (comp/transact! app [(add-schema-property {:schema/name "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2"})])
;;; can also call "remote" and "rest" from here.

;;; Here is the query I would make: 
;;;[{[:sdb/schema-id 1229]
;;;  [{:schema/components
;;;    [:component/doc-string
;;;     :component/name
;;;     :component/type
;;;     :component/min-occurs
;;;     :component/max-occurs]}]}]

(defsc StructurePlaceholder [_ {:ui/keys [id]}]
  {:query [:ui/id]
   :ident :ui/id}
  (div :.ui.placeholder.segment
       (div :.ui.field
            (dom/label {:style {:color "red"}} "Make a selection above."))))

(def ui-structure-placeholder (comp/factory StructurePlaceholder))

(defsc SchemaStructure [_ {:sdb/keys [schema-id] :as props}]
  {:query [:sdb/schema-id {:schema/message-sequence (comp/get-query SchemaPart)}]
   :ident :sdb/schema-id
   :initial-state {:sdb/schema-id :placeholder}}
  (div :.ui.field
       (dom/label (str "Rendering schema " schema-id))))

(def ui-schema-structure (comp/factory SchemaStructure {:keyfn :sdb/schema-id}))
;;;======================================================================
;;; Keep this for my fulcro.org notes!
(defn make-person
  "Make a person data map with optional children."
  [id name children]
  (cond-> {:db/id id :person/name name}
    children (assoc :person/children children)))

(declare ui-person)

; The ... in the query means there will be children of the same type, of arbitrary depth
; it is equivalent to (comp/get-query Person), but calling get query on yourself would
; lead to infinite compiler recursion.
(defsc Person [this {:keys [:person/name :db/id :person/children] :as props}] ; POD added :db/id here
  {:query         (fn [] [:db/id :person/name {:person/children '...}]) ; db/id was always here (not my doing).
   :initial-state (fn [p]
                    (make-person 1 "Joe"
                      [(make-person 2 "Suzy" [])
                       (make-person 3 "Billy" [])
                       (make-person 4 "Rae"
                         [(make-person 5 "Ian"
                            [(make-person 6 "Zoe" [])])])]))
   :ident         :db/id #_[:person/id :db/id]} ; <============================ Bug here? Just a renaming???
  (log/info "Person props = " props)
    (dom/div :.ui.accordion ; :.ui.styled.accordion is not interesting; it puts a rectangle around it. 
    (dom/div :.active.title        ; :.active.title points it downward. 
       (dom/i :.dropdown.icon) ; gives it the arrow
       (str name id))
    (dom/div :.active.content  ; must be .active to show
       (dom/p #_:.transition.visible (str "The name is " name))
       (when (seq children) ; ul gives it the indentation.
         (dom/ul (map (fn [p] (ui-person p)) children)))))
  #_(dom/div
    (dom/h4 name)
    (when (seq children)
      (dom/div
        (dom/ul
          (map (fn [p]
                 (ui-person p))
            children))))))

(def ui-person (comp/factory Person {:keyfn :db/id}))

;;;======================================================================
(defsc SchemaListItem [_ {:keys [sdb/schema-id schema/name]}]
  {:query [:sdb/schema-id :schema/name] 
   :ident :sdb/schema-id}
  ;; Since mapping over these (see SourceSchema below), they should have a unique :key. 
  (dom/option {:key (str schema-id) :value (str schema-id)} name)) 

(def ui-schema-list-item (comp/factory SchemaListItem {:keyfn :sdb/schema-id}))

(def editor-props {:value "function myScript(){return 100;}\n"
                   :onChange (fn [_ _ text] (js/console.log (str "New text = " text)))
                   :options {:lineNumbers true
                             :inputStyle "contenteditable"
                             :theme "material" ;"ambiance"
                             :mode "javascript"}})

;;; (get-query SchemaPart) ==> [:schema-elem/id :schema/property-name :schema/min-occurs :schema/max-occurs]
;;; This is working in as far as the DB is updated, but it isn't causing SourceStructure to update. 
(fm/defmutation set-schema [{:sdb/keys [schema-id] :ui/keys[id]}]
  (action [{:keys [state]}]
          (let [ident [:sdb/schema-id schema-id]]
            (df/load! APP ident SchemaStructure)
            (case id
              ::source (swap! state (fn [s] (assoc s :root/source-schema ident)))
              ::target (swap! state (fn [s] (assoc s :root/target-schema ident)))))))

;;; POD Is there any point in adding :ui/id to app root? (I don't do it currently). I just push it on props.
(defsc StructureSwitcher [this {:list/keys [schemas] :ui/keys [id displaying] :as props}]
  {:query [:ui/id :ui/displaying {:list/schemas (comp/get-query SchemaListItem)}]
   :ident :ui/id
   :initial-state (fn [p] {:ui/id (:ui/id p) :ui/displaying [:sdb/schema-id :placeholder]})}
  (div
   (dom/select
    :.ui.search.dropdown
    {:onChange (fn [arg]
                 (comp/transact!
                  this
                  `[(set-schema {:sdb/schema-id ~(js/parseInt (.. arg -target -value))
                                 :ui/id ~id})]))}
    (into [(dom/option {:key 0 :value ""}
                       (if (= id ::source)
                         "Select Source Schema"
                         "Select Target Schema"))]
          (mapv ui-schema-list-item schemas)))
   (if (= [:sdb/schema-id :placeholder] displaying)
     (ui-structure-placeholder {})
     (case id
       ::source (ui-schema-structure {:sdb/schema-id (-> APP ::app/state-atom deref :root/source-schema second)})
       ::target (ui-schema-structure {:sdb/schema-id (-> APP ::app/state-atom deref :root/target-schema second)})))))

(def ui-structure-switcher (comp/factory StructureSwitcher {:keyfn :ui/id}))

(defsc Root [_ {:keys [person-of-interest] :as props}]
  {:query [{:root/menu-source [:list/id {:list/schemas [:sdb/schema-id :schema/name]}]}
           {:root/source-schema (comp/get-query StructureSwitcher {:ui/id ::source})} ; POD just a guess!
           {:root/target-schema (comp/get-query StructureSwitcher {:ui/id ::target})}
           {:person-of-interest (comp/get-query Person)}]
   :initial-state (fn [_] {:root/source-schema (:ui/displaying (comp/get-initial-state StructureSwitcher {:ui/id ::source})),
                           :root/target-schema (:ui/displaying (comp/get-initial-state StructureSwitcher {:ui/id ::target})),
                           :person-of-interest (comp/get-initial-state Person)})}
  (div :.ui.container ; This supposedly restricts the size to 1127px on desktops; it does restrict, but not that much.
       (ui-goal-modal {:ui/modal-id "TenQuestions"})
       (div :.ui.two.column.grid
            (div :.ui.column ; value of :root/menu-source is the *map* described by the query!
                   (dom/div :.treemenu.boxed (ui-person person-of-interest))
                 #_(ui-structure-switcher (-> (:root/menu-source props)
                                            (assoc :ui/id ::source)
                                            (assoc :person-of-interest (comp/get-query Person))
                                            (assoc :ui/displaying (:root/source-schema props)))))
            (div :.ui.column
                 (ui-structure-switcher (-> (:root/menu-source props)
                                            (assoc :ui/id ::target)
                                            (assoc :ui/displaying (:root/target-schema props))))))
            ;; POD this codemirror instance will become a singleton component [:component/id ::Editor]
       (div :.ui.row (ui-code-mirror editor-props))
       (div :.ui.two.column.grid
            (div :.ui.column (ui-code-mirror {:value "source data"   :options {:lineNumbers true}}))
            (div :.ui.column (ui-code-mirror {:value "target output" :options {:lineNumbers true}})))))


(defsc SchemaListInfo [_ {:list/keys [id]}]
  {:query [:list/id {:list/schemas (comp/get-query SchemaListItem)}]
   :ident (fn [] [:list/id id])})

;;; POD, this reloads initial-state on first call. Subsequent calls don't change the DB.
;;; Hypothesis about how the df/load! works:
;;;  SourceSchema (likewise TargetSchema) is identified (:ident) by :list/id, so that a query can be made:
;;;  [{:message-schema [:list/id {:list/schemas [:sdb/schema-id :schema/name]}]}]
;;;  The response from this query supplies the other argument, list/schema as evident from the query. 
(defn ^:export init []
  (app/mount! APP Root "app")
  ;; I know I'll still want to go back and investigate why this isn't called with [:list/id :message-schema]
  ;; but it follows the pattern of the "friends" list demo in the book. 
  (df/load! APP :message-schema SchemaListInfo {:target [:root/menu-source]}) ; First arg is instance, third is ComponentType
  (js/console.log "Loaded"))

;;; POD This defines what is to be done when the file is saved.
(defn ^:export refresh []
  ;; re-mounting will cause forced UI refresh
  (app/mount! APP Root "app")
  ;; 3.3.0+ Make sure dynamic queries are refreshed
  (comp/refresh-dynamic-queries! APP)
  (js/console.log "Hot reload"))