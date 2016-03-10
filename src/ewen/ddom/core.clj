(ns ewen.ddom.core)

(defmacro defnx [name args & body]
  `(do
     (defn ^:export ~name ~args
       ~@body)
     (cljs.core/specify! ~name
       IXNamed
       (xname [~'_]
         (str (munge ~(str *ns*)) "." (-> ~name quote munge str)))
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
           (str (munge ~(str *ns*)) "." (-> ~name quote munge str)))
         cljs.core/IPrintWithWriter
         (cljs.core/-pr-writer [~'o ~'writer ~'_]
           (cljs.core/-write
            ~'writer
            (str "#ewen.ddom.core/exported " (pr-str (xname ~'o))))))))
