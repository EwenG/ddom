(ns ewen.ddom.core
  (:require [cljs.compiler :as comp]))

(defmacro defnx [name args & body]
  `(do
     (defn ^:export ~name ~args
       ~@body)
     (cljs.core/specify! ~name
       IXNamed
       (xname [~'_]
         ~(str (comp/munge (str *ns*)) "."
               (comp/munge (str name))))
       cljs.core/IPrintWithWriter
       (cljs.core/-pr-writer [~'o ~'writer ~'_]
         (cljs.core/-write
          ~'writer
          (str "#ewen.ddom.core/exported " (pr-str (xname ~'o))))))))

(defmacro defx [name x]
  `(do (def ^:export ~name ~x)
       (cljs.core/specify! ~name
         IXNamed
         (xname [~'_]
           ~(str (comp/munge (str *ns*)) "."
                 (comp/munge (str name))))
         cljs.core/IPrintWithWriter
         (cljs.core/-pr-writer [~'o ~'writer ~'_]
           (cljs.core/-write
            ~'writer
            (str "#ewen.ddom.core/exported " (pr-str (xname ~'o))))))))
