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
  (^string xname [x]
   "Returns the munged fully qualified name String of x."))

;; Clojure (jvm) specific type. This type exists only in order to be able
;; to extend print-method to implementations of IXNamed in a single place.
;; Without it, we would have to use reify and print-method would have to be
;; extended to every type created by reify.
#?(:clj (deftype XNamed [ns name]
          IXNamed
          (xname [_] (munge-namespaced ns name))))

;; Clojure (jvm) specific XNamed printing
#?(:clj (defmethod print-method XNamed [o ^java.io.Writer w]
          (.write
           w (str "#ewen.ddom.core/exported " (pr-str (xname o))))))

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
          (let [[docstring macro-args] (if (string? (first macro-args))
                                         [(first macro-args) (next macro-args)]
                                         [nil macro-args])
                [attr macro-args]          (if (map? (first macro-args))
                                             [(first macro-args) (next macro-args)]
                                             [{} macro-args])
                attr                       (if docstring
                                             (assoc attr :doc docstring)
                                             attr)
                attr                       (if (meta name)
                                             (conj (meta name) attr)
                                             attr)]
            [(with-meta name attr) macro-args])))

#?(:clj (defn specify-exported [name]
          `(cljs.core/specify! ~name
             IXNamed
             (xname [~'_]
               ~(str (comp/munge (str *ns*)) "."
                     (comp/munge (str name))))
             cljs.core/IPrintWithWriter
             (cljs.core/-pr-writer [~'o ~'writer ~'_]
               (cljs.core/-write
                ~'writer
                (str "#ewen.ddom.core/exported " (pr-str (xname ~'o))))))))

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
  #ewen.ddom.core/exported tag reader, which make reading it back possible"
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
  protocol and it is printed using the #ewen.ddom.core/exported tag reader,
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

;; A cljs tag reader for exported definitions, ie: things defined with defx
;; or defnx
#?(:cljs
   (reader/register-tag-parser! 'ewen.ddom.core/exported
                                (fn [x]
                                  (apply aget js/window (split x ".")))))

(comment
  #?(:cljs
     (read-string "#ewen.ddom.core/exported ewen.ddom.core.read_string_x"))
  )

(defn handler [xfn & params]
  "Converts a function implementing the IXNamed protocol into a string of
javascript code. The javascript code is a call to this function with params
 as parameters. The parameters must all be serializable through pr-str and
readable through cljs.reader/read-string"
  {:pre [(satisfies? IXNamed xfn)]}
  (let [full-name (xname xfn)
        format-param (fn [param]
                       (let [param-s (pr-str (pr-str param))]
                         (format "%s.call(null,%s)"
                                 (xname read-string-x)
                                 param-s)))
        params (if-not (empty? params)
                 (str "," (join "," (map format-param params)))
                 "")]
    (format "%s.call(null,event%s)" full-name params)))

#?(:cljs (def string->fragment
           "An alias for goog.dom/htmlToDocumentFragment. Converts a string
  of HTML into dom node(s)."
           dom/htmlToDocumentFragment))

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
