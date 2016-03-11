(ns ewen.ddom.core
  (:require #?@(:cljs
                [[hiccup.core :refer-macros [html]]
                 [hiccup.def :refer-macros [defhtml]]
                 [hiccup.page :refer [include-css]]
                 [goog.dom :as dom]
                 [cljs.reader :as reader :refer [read-string]]])
            #?(:clj [cljs.compiler :as comp])
            [clojure.string :refer [split join]])
  #?(:cljs (:require-macros [ewen.ddom.core :refer [defnx]]))
  #?(:cljs (:import [goog.string format])))

;; Cross platform protocol
(defprotocol IXNamed
  "Protocol for retrieving name of an exported def"
  (^string xname [x]
   "Returns the munged fully qualified name String of x."))

;; Clojure (jvm) specific type
#?(:clj (deftype XNamed [name]
          IXNamed
          (xname [_]
            (str (comp/munge (str *ns*)) "."
                 (comp/munge (str name))))))

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

#?(:clj (defmacro defnx [name & meta-body]
          (let [[name body] (name-with-attributes name meta-body)]
            (if (cljs-env? &env)
              `(do (defn ^:export ~name ~@body)
                   ~(specify-exported name))
              `(def ~name ~(->XNamed name))))))

#?(:clj (defmacro defx [name & meta-body]
          (let [[name body] (name-with-attributes name meta-body)]
            `(do (def ^:export ~name ~@body)
                 ~(specify-exported name)))))

;; End of macros

;; Same as the standard read-string function, but exported
(defnx read-string-x [s]
  (read-string s))

;; Cljs tag reader for exported "vars", defined with defx or defnx
#?(:cljs
   (reader/register-tag-parser! 'ewen.ddom.core/exported
                                (fn [x]
                                  (apply aget js/window (split x ".")))))

(comment
  #?(:cljs
     (read-string "#ewen.ddom.core/exported ewen.ddom.core.read_string_x"))
  )

(defn handler [xfn & params]
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
