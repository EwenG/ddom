(ns ewen.ddom.core
  (:require [hiccup.core :refer-macros [html]]
            [hiccup.def :refer-macros [defhtml]]
            [hiccup.page :refer [include-css]]
            [goog.dom :as dom]
            [clojure.string :refer [join]])
  (:import [goog.string format]))

(defn fn-name [ns fn-name]
  (str (munge ns) "." (munge fn-name)))

(defn handler [ns]
  (fn [fn-n & params]
    (let [fn-n (fn-name ns fn-n)
          params (if-not (empty? params)
                   (str "," (join "," (map pr-str params)))
                   "")]
      (format "%s.call(null,event%s)" fn-n params))))
