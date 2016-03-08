(ns ewen.ddom.core
  (:require [hiccup.core :refer-macros [html]]
            [hiccup.def :refer-macros [defhtml]]
            [hiccup.page :refer [include-css]]
            [goog.dom :as dom]
            [clojure.string :refer [join]])
  (:require-macros [ewen.ddom.core :refer [defnx]])
  (:import [goog.string format]))

(defprotocol IXNamed
  "Protocol for retrieving name of an exported def"
  (^string xname [x]
   "Returns the munged name String of x.")
  (^string xnamespace [x]
   "Returns the munged namespace String of x.")
  (^string xfullname [x]
   "Returns the munged fully qualified name String of x."))

(defn handler* [xfn & params]
  (let [full-name (xfullname xfn)
        params (if-not (empty? params)
                 (str "," (join "," (map pr-str params)))
                 "")]
    (format "%s.call(null,event%s)" full-name params)))

(defn fn-name [ns fn-name]
  (str (munge ns) "." (munge fn-name)))

(defn handler [ns]
  (fn [fn-n & params]
    (let [fn-n (fn-name ns fn-n)
          params (if-not (empty? params)
                   (str "," (join "," (map pr-str params)))
                   "")]
      (format "%s.call(null,event%s)" fn-n params))))
