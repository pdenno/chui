(ns app.model.mmp
  "Parse the message mapping language."
  (:require
   [app.model.util :as util]
   [app.model.schema-db :as db] 
   [clojure.pprint :refer (cl-format)]
   [clojure.string :as str]
   [clojure.set    :as sets]
   [clojure.spec.alpha :as s]
   [taoensso.timbre :as log]))

;;; The 'defparse' parsing functions pass around complete state. 
;;; The 'parse state' (AKA pstate) is a map with keys:
;;;   :result  - the parse structure from the most recent call to (parse :<some-rule-tag> pstate)
;;;   :tokens  - tokenized content that needs to be parsed into :model. First on this vector is also :tkn.
;;;   :stack   - a stack of tags indicating where in the grammar it is parsing (used for debugging)
;;;   :tkn     - current token, not yet consumed. It is also the first token on :tokens. 
;;;   :line    - line in which token appears.
;;;   :col     - column where token starts. 
;;;   :local   - temporarily stored parse content used later to form a complete grammar element.
;;;              It is a vector (stack) of maps. On entry, defparse pushes a new empty map on it; 
;;;              on exit, it pops it. Macros store and recall push onto the top map of the stack.
;;;              For example use, see ::relation and parse-list-terminated. 

(def debugging? (atom true))
(def diag (atom nil))
(def small (slurp "./data/map-examples/small.mmap"))

;;; ============ Tokenizer ===============================================================
;;; POD All this lexer stuff could be integrated with the :builtin stuff of the parser!
(def mm-keywords-basic
  #{"alias" "and" "else" "elseif" "endif" "false" "for" "function" "if" "in" "int" "library" "list" "metadata"
    "of" "or" "relation" "return" "source" "string" "target" "then" "top" "transformation" "true" "where"})

(def mm-keywords-function  #{"type" "children" "map" "filter" "reduce" "range"})

(def mm-keywords (sets/union mm-keywords-basic mm-keywords-function))

(def mm-builtin-type #{:int :string})

(def ^:private mm-syntactic ; chars that are valid tokens in themselves. 
  #{\[, \], \(, \), \{, \}, \=, \,, \., \:, \;, \*, \+, \/, \-, \<, \> \%})

(def ^:private mm-long-syntactic ; chars that COULD start a multi-character syntactic elements. 
  #{\<, \>, \=}) ; Don't put eol-comment (//) here.

(defrecord MmOp [name])
(defrecord MmId [name])
(defrecord MmEOLcomment [text])

;;; POD multi-line comment (e.g. /* ... */ would go in here, sort of. 
(defn read-long-syntactic [st ws]
  (let [len (count st)
        c0  (nth st 0)
        c1  (and (> len 1) (nth st 1))]
    (when-let [result (cond (and (= c0 \/) (= c1 \/)) {:raw "//" :tkn :eol-comment}
                            (and (= c0 \<) (= c1 \=)) {:raw "<=" :tkn :<=}
                            (and (= c0 \>) (= c1 \=)) {:raw ">=" :tkn :>=}
                            (and (= c0 \=) (= c1 \=)) {:raw "==" :tkn :==}
                            (and (= c0 \!) (= c1 \=)) {:raw "!=" :tkn :!=})]
      (assoc result :ws ws))))

(defn position-break 
  "Return the first position in s containing a syntactic character, ws,
   or nil if it contains none."
  [s]
  (let [len (count s)]
    (loop [n 0]
      (let [c (get s n)]
        (cond
          (= len n) nil
          (mm-long-syntactic c) n ; POD swapped with mm-syntactic
          (mm-syntactic c) n
          (#{\space \tab \newline} c) n
          :else (recur (inc n)))))))

(defn whitesp 
  "Evaluates to whitespace at head of string or empty string if none."
  [s] ; https://stackoverflow.com/questions/15020669/clojure-multiline-regular-expression
  (if s (or (nth (re-matches #"(?s)(\s+).*$" s) 1) "") ""))

;;; https://www.regular-expressions.info/modifiers.html (?s) allows  .* to match all characters including line breaks. 
(defn token-from-string
  "Return a map with keys :ws, :raw and :tkn from the front of the argument string."
  [stream line]
  (let [ws (whitesp stream)
        s (subs stream (count ws))
        c (first s)]
    ;(cl-format *out* "~%ws = ~S~%c = ~S~%STREAM = ~S" ws c stream)
    (or  (and (empty? s) {:ws ws :raw "" :tkn :eof})                    ; EOF
         (when-let [[_ cm] (re-matches #"(?s)(\/\/[^\n]*).*" s)]        ; EOL comment
           {:ws ws :raw cm :tkn (->MmEOLcomment cm)})
         (and (mm-long-syntactic c) (read-long-syntactic s ws))         ; ++, <=, == etc. 
         (and (mm-syntactic c) {:ws ws :raw (str c) :tkn c})            ; literal syntactic char.
         (when-let [[_ num] (re-matches #"(?s)(\d+(\.\d+(e[+-]?\d+)?)?).*" s)]
           {:ws ws :raw num :tkn (read-string num)}),                   ; number
         (when-let [[_ st] (re-matches #"(?s)(\"[^\"]*\").*" s)]        ; string literal
           {:ws ws :raw st :tkn (read-string st)})
         (when-let [[_ tivar] (re-matches #"(?s)(\$[A-Za-z][A-Za-z0-9_]*)" s)]
           {:ws ws :raw tivar :tkn {:type :type-inst-var :name tivar}})
         (let [pos (position-break s)
               word (subs s 0 (or pos (count s)))]
            (or 
             (and (mm-keywords word) {:ws ws :raw word :tkn (keyword word)})  
             (when-let [[_ id] (re-matches #"^([a-zA-Z][A-Za-z0-9\_\?]*).*" word)]     ; identifer
               {:ws ws :raw id :tkn (->MmId id)})))
         (throw (ex-info "Char starts no known token: " {:raw c :line line})))))

(defn tokenize
  "Return a vector of tokens. A token is a map with keys :tkn, :line :col."
  [stream]
  (loop [s stream 
         tkns []
         line 1
         col 1]
    (let [lex (token-from-string s line) ; Returns a map with keys :ws :raw and :tkn.
          new-lines (count (re-seq #"\n" (:ws lex))) ; :ws is in front of token. 
          col (if (> new-lines 0)
                (- (count (:ws lex)) (str/last-index-of (:ws lex) "\n"))
                (+ (count (:ws lex)) col))]
      (if (= :eof (:tkn lex))
        (conj tkns {:tkn :eof :line line :col col})
        (recur
         (subs s (+ (count (:raw lex)) (count (:ws lex))))
         (conj tkns {:tkn (:tkn lex) :line (+ line new-lines) :col col})
         (+ line new-lines)
         (+ col (count (:raw lex))))))))

;;; (parse-string example)
(def example (slurp "data/map-examples/BPC2OAGIS-example.mmap"))

;;; ============ Parser Utilities ============================================================
(defn look
  "Returns a token, not the pstate."
  [pstate n]
  (if (>= n (count (:tokens pstate)))
    :eof
    (-> (nth (:tokens pstate) n) :tkn)))

(defn match-tkn
  "Return true if token matches test, which is a string, character, fn or regex."
  [test tkn]
  (cond (= test tkn) true
        (set? test) (test tkn)
        (fn? test) (test tkn)
        (instance? java.util.regex.Pattern test) (re-matches test tkn)
        :else false))

(defn eat-token-aux
  "The actual work of eating a token."
  [pstate]
  (let [next-up (-> pstate :tokens second)]
    (-> pstate
        (assoc :tkn  (or (:tkn next-up) :eof))
        (assoc :line (:line next-up))
        (assoc :col  (:col next-up))
        (assoc :tokens (vec (rest (:tokens pstate)))))))

(defn eat-token
  "Move head of :tokens to :tkn ('consuming' the old :tkn) With 2 args, test :tkn first."
  ([pstate] (eat-token-aux pstate))
  ([pstate test]
   (if (match-tkn test (:tkn pstate))
       (eat-token-aux pstate)
       (throw (ex-info "eat-token test failed" {:test test :tkn (:tkn pstate) :pstate pstate})))))

(defn token-vec [pstate] (mapv :tkn (:tokens pstate)))

(def balanced-map "What balances with the opening syntax?" { \{ \}, \( \), \[ \] :2d-array-open :2d-array-close})
(def balanced-inv (sets/map-invert balanced-map))
  
(defn find-token
  "Return position if tkn is found within the item (before semicolon)."
  ([tvec tkn] (find-token tvec tkn #{\; :where}))
  ([tvec tkn stop-tokens]
   (when (not-empty tvec)
     (let [pos-stop (->> (map #(.indexOf tvec %) stop-tokens) (apply max))
           pos-stop (if (pos? pos-stop) pos-stop (count tvec)) ; In testing, might not have full item; not stop. 
           pos-tkn  (.indexOf tvec tkn)]
       (cond (== pos-tkn  -1) nil,
             (and (pos? pos-stop) (< pos-stop pos-tkn)) nil,
             :else pos-tkn)))))

;;; (find-token-balanced [ \{, \{, :foo, \}, \}, ] \}) ==> 4
(defn find-token-balanced
  "Return the position of a balanced instance of the argument token (a close-syntax token).
   Thus if tvec is [ \\{, \\{, foo, \\}, \\}, ] it is 4, not 3. Return nil if none."
  [tvec close-tkn]
  (when-let [open-tkn (balanced-inv close-tkn)]
    (assert (= open-tkn (first tvec)))
    (loop [cnt 1
           pos 0
           tvec (rest tvec)]
      (cond (== 0 cnt) pos
            (empty? tvec) nil
            :else
            (let [tkn (first tvec)]
              (recur (cond (= tkn open-tkn)  (inc cnt)
                           (= tkn close-tkn) (dec cnt)
                           :else cnt)
                     (inc pos)
                     (rest tvec)))))))

(defn balanced? 
  "Return true if, before position POS there is a closing syntax character for each 
  argument TKN opening syntax character." 
  [tvec open-tkn pos]
  (let [close-tkn (get balanced-map open-tkn)]
    (== 0 (reduce (fn [cnt tkn]
                    (cond (= tkn open-tkn) (inc cnt)
                          (= tkn close-tkn) (dec cnt)
                          :else cnt))
                  0
                  (subvec tvec 0 pos)))))

(defmacro defparse [tag [pstate & keys-form] & body]
  `(defmethod parse ~tag [~'tag ~pstate ~@(or keys-form '(& _))]
     (when @debugging? (cl-format *out* "~%~A==> ~A" (util/nspaces (* 3 (-> ~pstate :stack count))) ~tag))
     (if (<= (:call-count ~pstate) (:max-calls ~pstate))
       (as-> ~pstate ~pstate
         (update ~pstate :call-count inc)
         (update ~pstate :stack conj ~tag)
         (update ~pstate :local #(into [{:locals-for ~tag}] %))
         ~@body
         (if (not-empty (:stack ~pstate)) (update ~pstate :stack pop) ~pstate)
         (update ~pstate :local #(vec (rest %)))
         (do (when @debugging? (cl-format *out* "~%~A<-- ~A   ~S"
                                          (util/nspaces (* 3 (-> ~pstate :stack count)))
                                          ~tag
                                          (:result ~pstate)))
             ~pstate))
       (throw (ex-info "Exceeded parse call-count (Bug in a defparse?)." {:pstate ~pstate})))))

;;; Abbreviated for simple forms such as builtins. 
(defmacro defparse-auto [tag test]
  (let [pstate# nil]
    `(defparse ~tag
       [pstate#]
       (-> pstate#
           (assoc :result (:tkn pstate#))
           (eat-token pstate# ~test)))))

;;; This is an abstraction to protect :result while something else is swapped in.
;;; The 'from' is what key of ps to take from (defaults to :result). 
(defmacro store [ps key & [from]]
  `(let [ps# ~ps
         key# ~key]
     (assoc-in ps# [:local 0 key#]
               (~(or from :result) ps#))))

;;; ...and this is for getting the value back. 
(defmacro recall [ps tag]
  `(let [ps# ~ps]
     (-> ~ps :local first ~tag)))

(defn parse-dispatch [tag & _] tag)

(defmulti parse #'parse-dispatch)

(defn make-pstate
  "Make a parse state map from tokens, includes separating comments from code."
  [tokens+comments]
  (let [tokens   (remove #(instance? MmEOLcomment (:tkn %)) tokens+comments)
        comments (filter #(instance? MmEOLcomment (:tkn %)) tokens+comments)]
  {:tokens (vec tokens)
   :tkn (-> tokens first :tkn)
   :stack []
   :local []
   :call-count 0
   :max-calls (* (count tokens) 20)
   :comments comments}))

(defn parse-string
  "Toplevel parsing function"
  ([str] (parse-string ::transformation str))
  ([tag str]
   (let [pstate (->> str tokenize make-pstate (parse tag))]
     (if (not= (:tkn pstate) :eof)
       (do (when @debugging? (cl-format *out* "~2%*** Tokens remain:~%"))
           pstate)
       pstate))))

(defn parse-file
  "Parse a whole file given a filename string."
  [filename]
  (parse-string ::transformation (slurp filename)))

(defn parse-ok?
  "Return true if the string parses okay."
  [tag text]
  (as-> (parse-string tag text) ?pstate
    (and (= :eof (-> ?pstate :tokens first :tkn))
         (or (not (contains? (s/registry) tag))
             (s/valid? tag (:result ?pstate))))))

;;;========================= Implementation of Grammar ==========================
(defn parse-list
  "Does parse parametrically for <open-char> [ <item> <char-sep>... ] <close-char>"
  ([pstate char-open char-close char-sep]
   (parse-list pstate char-open char-close char-sep ::exp))
  ([pstate char-open char-close char-sep parse-tag]
   (when @debugging?
     (cl-format *out* "~%>>>>>>>>>>>>>> parse-list (~A) >>>>>>>>>>>>>>>>>>" parse-tag))
   (let [final-ps
         (as-> pstate ?ps
           (eat-token ?ps char-open)
           (assoc-in ?ps [:local 0 :items] [])
           (loop [ps ?ps]
             (cond
               (= :eof (:tkn ps))
               (throw (ex-info "parsing a list" {:tag parse-tag :pstate ps})),
               (= char-close (:tkn ps))
               (as-> ps ?ps1
                 (eat-token ?ps1)
                 (assoc ?ps1 :result (recall ?ps1 :items))),
               :else
               (as-> ps ?ps1
                 (parse parse-tag ?ps1)
                 (update-in ?ps1 [:local 0 :items] conj (:result ?ps1))
                 (recur (cond-> ?ps1 (= char-sep (:tkn ?ps1)) (eat-token char-sep)))))))]
     (when @debugging?
       (println "\nCollected" (:result final-ps))
       (println "\n<<<<<<<<<<<<<<<<<<<<< parse-list <<<<<<<<<<<<<<<<<<<<<<<"))
     final-ps)))

(defn parse-list-terminated
  "Does parse parametrically for '[ <item> ','... ] <terminator>'. Does not eat terminator."
  [pstate & {:keys [term-fn sep-fn parse-tag] :or {sep-fn #(= \; %)
                                                   term-fn #(= \} %)
                                                   parse-tag ::map-stmt}}]
  (when @debugging?
    (cl-format *out* "~%>>>>>>>>>>>>>> parse-list-terminated (~A) >>>>>>>>>>>>>>>>>>" parse-tag))
  (let [final-ps
        (as-> pstate ?ps
          (assoc-in ?ps [:local 0 :items] [])
          (loop [ps ?ps]
            (cond
              (= :eof (:tkn ps))  (throw (ex-info "parsing a terminated list" {:tag parse-tag :pstate ps}))
              (term-fn (:tkn ps)) (assoc ps :result (recall ps :items)),
              :else
              (as-> ps ?ps
                (parse parse-tag ?ps)
                (update-in ?ps [:local 0 :items] conj (:result ?ps))
                (recur (cond-> ?ps (sep-fn (:tkn ?ps)) (eat-token #{\,\;})))))))]
    (when @debugging?
      (println "\nCollected" (:result final-ps))
      (println "\n<<<<<<<<<<<<<<<<< parse-list-terminated <<<<<<<<<<<<<<<<"))
    final-ps))

;;; <builtin-num-bin-op> ::= + | - | * | / | div | mod
(def builtin-num-bin-op #{\+ \- \* \/ \% :div :mod})
(defparse-auto ::builtin-num-bin-op builtin-num-bin-op)

;;;  <builtin-bin-op> ::= <-> | -> | <- | \/ | xor | /\ | < | > | <= | >= | == | = | != | in 
(def builtin-bin-op
  (into #{\. :or-op :and-op \< \> :<= :>= :== \= :not= :in}
        builtin-num-bin-op))
(defparse-auto ::builtin-bin-op builtin-bin-op)

;;; <builtin-num-un-op> ::= + | -
(def builtin-num-un-op #{\+, \-})
(defparse-auto ::builtin-num-un-op builtin-num-un-op)

;;; <builtin-un-op> ::= "not" | <builtin-num-un-op>
(def builtin-un-op (conj builtin-num-un-op :not))
(defparse-auto ::builtin-un-op builtin-un-op)

;;; <builtin-op> ::= <builtin-bin-op> | <builtin-un-op>
(def builtin-op
  (sets/union builtin-bin-op
              builtin-un-op))
(defparse-auto ::builtin-op builtin-op)

;;; <builtin-fn> ::= "type" | "children" | "map" | "filter" | "reduce" | "range"
(def builtin-fn #{:type :children :map :filter :reduce :range})
(defparse-auto ::builtin-fn builtin-fn)

(defn mm-id? [x] (instance? MmId x))

;;;=============================== Message Mapper Grammar ===============================

(s/def ::transformation (s/keys :req-un [::relations]))
(defrecord MmTransformation [name preamble global-vars relations functions])
;;; <transformation> ::= "transformation" <id> \{ <preamble> <global-params>* <rel-or-fn>* \}
(defparse ::transformation ; top-level grammar element. 
  [pstate] 
  (as-> pstate ?ps
    (eat-token ?ps :transformation)
    (parse ::ident ?ps)
    (store ?ps :trans-name)
    (eat-token ?ps \{)
    (parse ::preamble ?ps)
    (store ?ps :preamble)
    (assoc-in ?ps [:local 0 :param-decls] []) ; explicitly set up to accumulate :result below.
    (loop [ps ?ps]
      (if (#{:top :relation} (:tkn ps))
        ps 
        (recur (as-> ps ?ps1
                 (parse ::id-type-pair ?ps1)
                 (eat-token ?ps1 \;)
                 (update-in ?ps1 [:local 0 :param-decls] conj (:result ?ps1))))))
    (loop [ps ?ps]
      (if (not (#{:top :relation} (:tkn ps))) ; POD should be #{:top :relation :function}
        ps
        (recur
         (let [ps (parse ::relation ps)]
           (update-in ps [:local 0 :relations] conj (:result ps))))))
    (eat-token ?ps \})
    (assoc ?ps :result (->MmTransformation (recall ?ps :trans-name)
                                           (recall ?ps :preamble)
                                           (recall ?ps :param-decls)
                                           (recall ?ps :relations)
                                           nil))))

(def preamble-key? #{:source :target :library :metadata})
(defparse ::preamble
  [pstate]
  (as-> pstate ?ps
    (assoc-in ?ps [:local 0 :preamble-decls] [])
    (loop [ps ?ps]
      (if (not (preamble-key? (:tkn ps)))
        ps
        (recur
         (as-> ps ?ps1
           (cond (= (:tkn ps) :source)
                 (parse ::source ?ps1)
                 (= (:tkn ps) :target)
                 (parse ::target ?ps1)
                 (= (:tkn ps) :library)
                 (parse ::library ?ps1)
                 (= (:tkn ps) :metadata)
                 (parse ::metadata ?ps1))
           (update-in ?ps1 [:local 0 :preamble-decls] conj (:result ?ps1))))))
    (assoc ?ps :result (recall ?ps :preamble-decls))))

;;;(parse-string ::relation "relation bar (myArg : myType) { var myLocal : int = 0;}")
;;; <relation> ::= "top"? "relation" <id> \( [<id-type-pair> \, ...]* \) <doc-string>? \{ [ <id-type-pair> \;...]* <map-stmt>* \}
(defrecord MmRelation [top? rel-name return-type doc-string rel-params domains map-stmts])
(defparse ::relation
  [pstate]
  (as-> pstate ?ps
    (if (= :top (:tkn ?ps)) (as-> ?ps ?ps1 (store ?ps1 :top? :tkn) (eat-token ?ps1)) ?ps)
    (eat-token ?ps :relation)
    (parse ::ident ?ps)
    (store ?ps :rel-name)
    (parse-list ?ps \( \) \, ::id-type-pair)
    (store ?ps :rel-params)
    (eat-token ?ps \:)
    (parse ::path-spec ?ps)
    (store ?ps :return-type)
    (if (string? (:tkn ?ps))
      (as-> ?ps ?ps1 (store ?ps1 :doc-string :tkn) (eat-token ?ps1))
      ?ps)
    (parse ::relation-domains ?ps)
    (eat-token ?ps \{)
    (parse-list-terminated ?ps) ; default tag=::map-stmt, sep=\; term=\}
    (store ?ps :map-stmts)
    (eat-token ?ps \})
    (assoc ?ps :result (->MmRelation (recall ?ps :top?)
                                     (recall ?ps :rel-name)
                                     (recall ?ps :return-type)
                                     (recall ?ps :doc-string)
                                     (recall ?ps :rel-params)
                                     (recall ?ps :rel-domains)
                                     (recall ?ps :map-stmts)))))

;;; relation-domains :== (<id> : <type)*
(defrecord MmRelationDomains [specs])
(defparse ::relation-domains
  [pstate]
  (as-> pstate ?ps
    (assoc-in ?ps [:local 0 :domains] [])
    (loop [ps ?ps]
      (if (not (mm-id? (:tkn ps)))
        ps ; This already has :domains stored. 
        (recur (as-> ps ?ps1
                 (parse ::id-type-pair ?ps1)
                 (eat-token ?ps1 \;)
                 (update-in ?ps1 [:local 0 :domains] conj (:result ?ps1))))))
    (assoc ?ps :result (->MmRelationDomains (recall ?ps :domains)))))

(defrecord MmSource [uri source-alias])
(defparse ::source
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :source)
    (store ?ps :uri :tkn)
    (eat-token ?ps string?)
    (eat-token ?ps \:)
    (parse ::ident ?ps)
    (eat-token ?ps \;)
    (assoc ?ps :result (->MmSource (recall ?ps :uri) (:result ?ps)))))

(defrecord MmTarget [uri target-alias])
(defparse ::target
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :target)
    (store ?ps :uri :tkn)
    (eat-token ?ps string?)
    (eat-token ?ps \:)
    (parse ::ident ?ps)
    (eat-token ?ps \;)
    (assoc ?ps :result (->MmTarget (recall ?ps :uri) (:result ?ps)))))

(defrecord MmLibrary [uri library-alias])
(defparse ::library
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :library)
    (store ?ps :uri :tkn)
    (eat-token ?ps string?)
    (eat-token ?ps \:)
    (parse ::ident ?ps)
    (eat-token ?ps \;)
    (assoc ?ps :result (->MmLibrary (recall ?ps :uri) (:result ?ps)))))

(defrecord MmMetadata [metadata-map])
(defparse ::metadata
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :metadata)
    (eat-token ?ps \{)
    (parse-list-terminated ?ps :term-fn #(= % \})
                           :sep-fn #(= % \,)
                           :parse-tag ::key-value-pair)
    (eat-token ?ps \})
    (eat-token ?ps \;)
    (assoc ?ps :result
           (->MmMetadata
            (zipmap (map #(-> % :key :name keyword) (:result ?ps))
                    (map :val (:result ?ps)))))))

(defrecord MmKeyValuePair [key val])    
(defparse ::key-value-pair
  [pstate]
  (as-> pstate ?ps
    (parse ::ident ?ps)
    (store ?ps :key)
    (eat-token ?ps \:)
    (store ?ps :val :tkn)
    (eat-token ?ps string?)
    (assoc ?ps :result (->MmKeyValuePair (recall ?ps :key) (recall ?ps :val)))))

(defrecord MmTypeDef [type-name])
(defparse ::type ; POD Needs work
  [pstate]
  (as-> pstate ?ps
      (assoc ?ps :result (->MmTypeDef (:tkn ?ps)))
      (eat-token ?ps #(or (mm-builtin-type %) (mm-id? %)))))

;;; POD not perfect, I do not want them to be assignable in the head of a map statement. 
;;; id-type-pair ::= <ident> \: <type> [\= <exp>]? 
(defrecord MmIdTypePair [id type init-val])
(defparse ::id-type-pair
  [pstate]
  (as-> pstate ?ps
    (parse ::ident ?ps)
    (store ?ps :param-id)
    (eat-token ?ps \:)
    (parse ::type ?ps)
    (store ?ps :param-type)
    (if (= \= (:tkn ?ps))
      (as-> ?ps ?ps1
        (eat-token ?ps1 \=)
        (parse ::exp ?ps1)
        (store ?ps1 :init-val))
      ?ps)
    (assoc ?ps :result (->MmIdTypePair (recall ?ps :param-id)
                                       (recall ?ps :param-type)
                                       (recall ?ps :init-val)))))

;;; POD This can be used for things that have a first path component that is an MmId, (a 'variable')
;;;     and a last path component that is a function or map call. Maybe rename it as '-generic'
;;;     Maybe in those cases, send in a :first and :last
;;; path-type-spec ::= [ <path-component> \. ...]+
(defrecord MmPathSpec [path-components])
(defparse ::path-spec
  [pstate]
  (as-> pstate ?ps
    (parse ::path-component ?ps)
    (assoc-in ?ps [:local 0 :components] [(:result ?ps)])
    (loop [ps ?ps]
      (if (not= (:tkn ps) \.)
        (assoc ps :result (->MmPathSpec (recall ps :components)))
        (recur (as-> ps ?ps1
                 (eat-token ?ps1)
                 (parse ::path-component ?ps1)
                 (update-in ?ps1 [:local 0 :components] conj (:result ?ps1))))))))

;;; POD like ::ident, but will someday include type checking
;;; (unless I go with 'path-spec--generic' in which case it will be done by the caller). 
(defrecord MmPathComponent [step-name])
(defparse ::path-component
  [pstate]
  (as-> pstate ?ps
    (assoc ?ps :result (->MmPathComponent (-> ?ps :tkn :name)))
    (eat-token ?ps mm-id?)))

;;; map-stmt ::= <map-call> | <map-target-assignment> | <map-comprehension>
(defparse ::map-stmt 
  [pstate]
  (let [tvec (token-vec pstate)
        =-pos (find-token tvec \=)
        open-pos (find-token tvec \()]
    (cond (= (:tkn pstate) \[) (parse ::map-comprehension pstate),
          (or (and =-pos open-pos (< =-pos open-pos)) =-pos) (parse ::map-target-assigment pstate),
          :else (parse ::map-call pstate))))

;;; This should return a <call-exp> at the end of path.
(defparse ::map-call
  [pstate]
  (parse ::exp pstate)) 

(defparse ::map-comprehension
  [pstate]
  (parse ::list-comprehension pstate))

      
;;;--------------------- exp ----------------------------------------------------------
(defn literal? [tkn]
  (or (string? tkn) (number? tkn) (#{:true :false} tkn)))

(defrecord MmExp [atom tail])
(defrecord MmExpUnOp [uni-op atom])

(defrecord MmExpAtom [head tail])
;;; <exp> ::= <expr-atom> <expr-binop-tail>
(defparse ::exp
  [pstate]
  (as-> pstate ?ps
    (parse ::exp-atom ?ps)
    (store ?ps :atom)
    (if (builtin-bin-op (:tkn ?ps))
      (as-> ?ps ?ps1
        (parse ::exp-binop-tail ?ps1)
        (store ?ps1 :tail))
      (assoc-in ?ps [:local 0 :tail] nil))
    (assoc ?ps :result (->MmExp (recall ?ps :atom) (recall ?ps :tail)))))

;;; <expr-atom> ::= <expr-atom-head> <expr-atom-tail> <annotations>
(defparse ::exp-atom
  [pstate]
  (as-> pstate ?ps
    (parse ::exp-atom-head ?ps)
    (store ?ps :head)
    (parse ::exp-atom-tail ?ps)
    (store ?ps :tail)
    (assoc ?ps :result (->MmExpAtom
                        (recall ?ps :head)
                        (recall ?ps :tail))))) 

;;; exp-atom-head ::= (<builtin-un-on> <exp>) | '(' <exp> ')' | <list-comprehension> | <if-exp> | <call-exp>
(defparse ::exp-atom-head
  [ps]
  (let [tkn (:tkn ps)
        tvec (token-vec ps)
        close-list-pos (and (= (first tvec) \[) (find-token-balanced tvec \]))
        tkn2 (look ps 1)]
    (cond (builtin-un-op tkn)                            ; <builtin-un-op> <exp>
          (as-> ps ?ps
            (eat-token ?ps)
            (parse ::exp ?ps)
            (assoc ?ps :result (->MmExpUnOp tkn (:result ?ps)))),
          (= \( tkn)                                     ; ( <exp> )
          (as-> ps ?ps
            (eat-token ?ps)
            (parse ::exp ?ps)
            (eat-token ?ps \))
            (assoc-in ?ps [:result :primary?] true))
          (and close-list-pos (find-token tvec :in)
               (< (find-token tvec :in) close-list-pos)) ; <list-comprehension>
          (parse ::list-comprehension ps),
          (= tkn :if) (parse ::if-exp ps),               ; <if-exp>
          (= tkn \[)  (parse ::list ps)
          (and (or (builtin-fn tkn) (mm-id? tkn)) (= \( tkn2)) ; <fn-call>
          (parse ::fn-call ps) 
          (mm-id? tkn)   (parse ::ident ps),
          (literal? tkn) (parse ::literal ps), 
          :else
          (throw (ex-info "expected an expression, unary-op, ident, [, (, or literal" 
                           {:got tkn :pstate ps})))))

(defrecord MmArrayAccess [exprs])
;;;  <expr-atom-tail> ::= ε | <array-access-tail> <expr-atom-tail>
(defparse ::exp-atom-tail
  [pstate]
  (if (= \[ (:tkn pstate))
    (as-> pstate ?ps
      (parse ::array-access-tail ?ps)
      (assoc ?ps :result (->MmArrayAccess (:result ?ps))))
    (assoc pstate :result nil)))

;;; <array-access-tail> ::= "[" <exp> "," ... "]"
(defparse ::array-access-tail
  [pstate]
  (parse-list pstate \[ \] \,) ::exp)

(defrecord MmExpBinopTail [bin-op expr])
;;; POD I think the grammar is botched here. The [ ] should be BNF optional, not terminals!
;;; <expr-binop-tail> ::= "[" <bin-op> <exp> "]"
(defparse ::exp-binop-tail
  [pstate]
  (as-> pstate ?ps
    (parse ::bin-op ?ps)
    (store ?ps :bin-op)
    (parse ::exp ?ps)
    (assoc ?ps :result (->MmExpBinopTail
                        (recall ?ps :bin-op)
                        (:result ?ps)))))

;;; <bin-op> ::= <builtin-bin-op> (see list of them)
(defparse ::bin-op
  [pstate]
  (let [tkn (:tkn pstate)]
    (if (builtin-bin-op tkn)
      (-> pstate (assoc :result tkn) eat-token)
      (throw (ex-info "expected a binary operator" {:got tkn})))))

;;;------------ 'atomic' expressions --------------------------------------
;;;(parse-string ::exp "x.foo(a)")
;;;(parse-string ::list-comprehension "[ x for x in range(20) where x % 2 == 0]")

;;; list-comprehension ::= '[' <exp> 'for' <id> 'in' <exp> ['where' <exp>]? ']'
(defrecord MmListComprehension [target-exp variable source filter])
(defparse ::list-comprehension
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps \[)
    (parse ::exp ?ps)
    (store ?ps :target-exp)
    (eat-token ?ps :for)
    (parse ::ident ?ps)
    (store ?ps :variable)
    (eat-token ?ps :in)
    (parse ::exp ?ps)
    (store ?ps :source)
    (if (= (:tkn ?ps) :where)
      (as-> ?ps ?ps1
        (eat-token ?ps1)
        (parse ::exp ?ps1)
        (store ?ps1 :filter))
      ?ps)
    (eat-token ?ps \])
    (assoc ?ps :result (->MmListComprehension (recall ?ps :target-exp)
                                              (recall ?ps :variable)
                                              (recall ?ps :source)
                                              (recall ?ps :filter)))))
(defparse ::ident
  [pstate]
  (let [tkn (:tkn pstate)]
    (if (mm-id? tkn)
      (-> pstate (assoc :result tkn) eat-token)
      (throw (ex-info "expected a identifier" {:got tkn})))))

(defparse ::list
  [pstate]
  (parse-list pstate \[ \] \, :exp))

(defparse ::literal
  [pstate]
  (let [tkn (:tkn pstate)]
    (if (literal? tkn)
      (-> pstate (assoc :result tkn) eat-token)
      (throw (ex-info "expected a literal" {:got tkn})))))

(defrecord MmElseIf [cond then])
(defrecord MmIfExp [condition then elseif else])
;;; <if-exp> ::= "if" <exp> "then" <exp> ("elseif" <exp> "then" <exp>)* "else" <exp> "endif"
(defparse ::if-exp
  [ps]
  (as-> ps ?ps
    (eat-token ?ps :if)
    (parse ::exp ?ps)
    (store ?ps :condition)
    (eat-token ?ps :then)
    (parse ::exp ?ps)
    (assoc-in ?ps [:local 0 :elseifs] [])
    (store ?ps :then)
    (if (= :elseif (:tkn ?ps))
      (loop [x ?ps]
        (as-> x ?ps1
          (eat-token ?ps1 :elseif)
          (parse ::exp ?ps1)       ; cond
          (store ?ps1 :elseif-cond)
          (eat-token ?ps1 :then)
          (parse ::exp ?ps1)       ; then
          (update-in ?ps1 [:local 0 :elseifs] conj (->MmElseIf (recall ?ps1 :elseif-cond) (:result ?ps1)))
          (if (= :elseif (:tkn ?ps1)) (recur ?ps1) ?ps1)))
      ?ps)
    (eat-token ?ps :else)
    (parse ::exp ?ps)
    (assoc ?ps :result (->MmIfExp (recall ?ps :condition)
                                  (recall ?ps :then)
                                  (recall ?ps :elseifs)
                                  (:result ?ps)))
    (eat-token ?ps :endif)))

;;; fn-call ::=  <ident> '(' [<exp> ','...]* ')'
(defrecord MmFnCall [fn-name args])
(defparse ::fn-call
  [ps]
  (as-> ps ?ps
    (if (builtin-fn (:tkn ?ps))
      (-> ?ps (store :fn-name :tkn) eat-token)
      (parse ::ident ?ps))
    (store ?ps :fn-name)
    (eat-token ?ps \()
    (parse-list-terminated ?ps :term-fn #(= % \)) :sep-fn #(= % \,) :parse-tag ::exp)
    (store ?ps :args)
    (eat-token ?ps \))
    (assoc ?ps :result (->MmFnCall (recall ?ps :fn-name) (recall ?ps :args)))))


