(ns ewen.ddom.core
  "A library for using the dom declaratively without relying on a virtual
  dom"
  (:require #?@(:cljs
                [[goog.dom :as dom]
                 [cljs.reader :as reader :refer [read-string]]])
            #?(:clj [cljs.compiler :as comp])
            [clojure.string :refer [split join]])
  #?(:cljs (:require-macros [ewen.ddom.core :refer [defnx]]))
  #?(:cljs (:import [goog.string format])))

#?(:clj (defn munge-namespaced [ns name]
          (str (comp/munge ns) "."
               (comp/munge name))))

;; Cross platform protocol
(defprotocol IXNamed
  "Protocol for retrieving name of an exported def"
  (^string xnamespace [x])
  (^string xname [x]))

;; Clojure (jvm) specific type. This type exists only in order to be able
;; to extend print-method to implementations of IXNamed in a single place.
;; Without it, we would have to use reify and print-method would have to be
;; extended to every type created by reify.
#?(:clj (deftype XNamed [ns name]
          IXNamed
          (xnamespace [_] (munge ns))
          (xname [_] (munge name))))

;; Clojure (jvm) specific XNamed printing
#?(:clj (defmethod print-method XNamed [o ^java.io.Writer w]
          (.write
           w (format "#ddom/x[%s %s]" (xnamespace o) (xname o)))))

;; Cross platform macros

;; Taken from tools.macro
;; https://github.com/clojure/tools.macro
#?(:clj (defn name-with-attributes
          "To be used in macro definitions.
   Handles optional docstrings and attribute maps for a name to be defined
   in a list of macro arguments. If the first macro argument is a string,
   it is added as a docstring to name and removed from the macro argument
   list. If afterwards the first macro argument is a map, its entries are
   added to the name's metadata map and the map is removed from the
   macro argument list. The return value is a vector containing the name
   with its extended metadata map and the list of unprocessed macro
   arguments."
          [name macro-args]
          (let [[docstring macro-args]
                (if (string? (first macro-args))
                  [(first macro-args) (next macro-args)]
                  [nil macro-args])
                [attr macro-args]
                (if (map? (first macro-args))
                  [(first macro-args) (next macro-args)]
                  [{} macro-args])
                attr
                (if docstring
                  (assoc attr :doc docstring)
                  attr)
                attr
                (if (meta name)
                  (conj (meta name) attr)
                  attr)]
            [(with-meta name attr) macro-args])))

#?(:clj (defn specify-exported [name]
          `(cljs.core/specify! ~name
             IXNamed
             (xnamespace [~'_]
               ~(str (comp/munge *ns*)))
             (xname [~'_]
               ~(str (comp/munge name)))
             cljs.core/IPrintWithWriter
             (cljs.core/-pr-writer [~'_ ~'writer ~'_]
               (cljs.core/-write
                ~'writer
                ~(format "#ddom/x[%s %s]"
                         (comp/munge *ns*) (comp/munge name)))))))

#?(:clj (defn cljs-env?
          "Take the &env from a macro, and tell whether we are expanding
  into cljs."
          [env]
          (boolean (:ns env))))

#?(:clj (defmacro defnx
          "Define an exported function. The exported function has its
  ^:exported metadata set to true which prevents it from being renamed by
  the closure advanced compilation mode. Its named can be queried using the
  IXNamed protocol and the function is printed using the
  #ddom/x tag reader, which make reading it back possible"
          [name & meta-body]
          (let [[name body] (name-with-attributes name meta-body)]
            (if (cljs-env? &env)
              `(do (defn ~name ~@body)
                   ~(specify-exported name)
                   ;; Manually export the symbol because specify seems
                   ;; to break the ^:export metadata
                   (goog/exportSymbol ~(munge-namespaced *ns* name) ~name))
              `(def ~name (->XNamed ~(str *ns*) ~(str name)))))))

#?(:clj (defmacro defx
          "Like def but set the ^:exported metadata to true, which prevents
  the thing defined from being renamed by the closure advanced compilation
  mode. The name of the thing defined can be queried using the IXNamed
  protocol and it is printed using the #ddom/x tag reader,
  which make reading it back possible"
          [name & meta-body]
          (let [[name body] (name-with-attributes name meta-body)]
            (if (cljs-env? &env)
              `(do (def ~name ~@body)
                   ~(specify-exported name)
                   ;; Manually export the symbol because specify seems
                   ;; to break the ^:export metadata
                   (goog/exportSymbol ~(munge-namespaced *ns* name) ~name))
              `(def ~name (->XNamed ~(str *ns*) ~(str name)))))))

;; End of macros

(defnx read-string-x
  "The same as the standard read-string function, but defined using defnx"
  [s]
  (read-string s))

#?(:cljs
   (defn parse-fn [ns name]
     (apply aget js/window (into (split ns ".") (split name ".")))))

;; A cljs tag reader for exported definitions, ie: things defined with defx
;; or defnx
#?(:cljs
   (reader/register-tag-parser!
    'ddom/x
    (fn [[ns name]] (parse-fn ns name))))

;; A cljs tag reader for event handlers
#?(:cljs
   (reader/register-tag-parser!
    'ddom/h
    (fn [[event-type ns name & params]]
      (into [(str event-type) (parse-fn ns name)] params))))

(comment
  #?(:cljs
     (read-string "#ddom/x[ewen.ddom.core read_string_x]")

     )

  #?(:cljs
     (read-string "#ddom/h[click ewen.ddom.core read_string_x]")
     )
  )

(def handler-key
  "The HTML parameter used by ddom to register event handlers"
  :data-ddom-event)

(defn handler [event-type xfn & params]
  "Takes an event type, a function implementing the IXNamed protocol and
returns a string containing the type of the event, the name of the
javascript function to be used to handle the event and optionally additional
parameters for the event hanlder. The parameters must all be serializable
through pr-str and readable through cljs.reader/read-string. The string is
formatted in a way suitable to be embedded in a \"data-ddom-event\" HTML
attribute for further processing by the ddom library."
  {:pre [(satisfies? IXNamed xfn)]}
  (let [params (if-not (empty? params)
                 (str " " (join " " (map (comp pr-str pr-str) params)))
                 "")]
    (format "#ddom/h[%s %s %s%s]"
            (name event-type) (xnamespace xfn) (xname xfn) params)))

#?(:cljs (def string->fragment
           "An alias for goog.dom/htmlToDocumentFragment. Converts a string
  of HTML into dom node(s)."
           dom/htmlToDocumentFragment))

(comment
  #?(:cljs (defn parse-params [rdr]
              (loop [rdr rdr
                     params (transient [])]
                (if-let [o (cljs.reader/read rdr false nil false)]
                  (recur rdr (conj! params o))
                  (persistent! params)))))

  #?(:cljs (defn parse-event-attr [event-attr]
              (loop [rdr (cljs.reader/push-back-reader event-attr)
                     event-type (cljs.reader/read rdr false nil false)
                     handler-fn (cljs.reader/read rdr false nil false)
                     params (parse-params rdr)]
                #js {:event-type event-type
                     :handler-fn handler-fn :params params}))))

#?(:cljs (defn parse-handlers [event-attr]
           (loop [rdr (cljs.reader/push-back-reader event-attr)
                  handlers (transient [])]
             (if-let [handler (cljs.reader/read rdr false nil false)]
               (recur rdr (conj! handlers handler))
               (persistent! handlers)))))

#?(:cljs
   (defn register-handlers [root]
     (let [nodes (.querySelectorAll root "[data-ddom-event]")
           root-attr (.getAttribute root "data-ddom-event")]
       (when (and root-attr (not= root-attr ""))
         (let [handlers (parse-handlers root-attr)]
           (doseq [[event-type handler-fn & params] handlers]
             (.addEventListener root
                                event-type
                                (fn [e] (apply handler-fn e params))
                                false))))
       (doseq [i (range (.-length nodes))]
         (let [node (aget nodes i)
               event-attr (.getAttribute node "data-ddom-event")
               handlers (parse-handlers event-attr)]
           (doseq [[event-type handler-fn & params] handlers]
             (.addEventListener node
                                event-type
                                (fn [e] (apply handler-fn e params))
                                false)))))))

#?(:cljs (defn replace-node
           ([new-root old-root]
            (replace-node new-root old-root nil))
           ([new-root old-root match-ids]
            {:pre [(or (nil? match-ids) (coll? match-ids))]}
            (when match-ids
              (doseq [id match-ids]
                (when-let [old-node (.querySelector
                                     old-root
                                     (format "[data-ddom-id=\"%s\"]" id))]
                  (when-let [new-node (.querySelector
                                       new-root
                                       (format "[data-ddom-id=\"%s\"]" id))]
                    (dom/replaceNode old-node new-node)))))
            (dom/replaceNode new-root old-root)
            new-root)))

(comment
  (require '[hiccup.core :refer-macros [html]])
  (require '[hiccup.def :refer-macros [defhtml]])
  (require '[hiccup.page :refer [include-css]])
  )
