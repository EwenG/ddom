(ns ewen.ddom.core)

(defmacro defnx [name args & body]
  `(do (defn ^:export ~name ~args
         ~@body)
       (cljs.core/specify! ~name
         IXNamed
         (xname [~'_] (-> ~name quote munge str))
         (xnamespace [~'_] (munge ~(str *ns*)))
         (xfullname [~'this] (str (xnamespace ~'this) "."
                                  (xname ~'this))))))

(defmacro defx [name x]
  `(do (def ^:export ~name ~x)
       (cljs.core/specify! ~name
         IXNamed
         (xname [~'_] (-> ~name quote munge str))
         (xfullname [~'this] (str (xnamespace ~'this) "."
                                  (xname ~'this))))))
