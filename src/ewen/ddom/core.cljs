(ns ewen.ddom.core
  (:require [hiccup.core :refer-macros [html]]
            [hiccup.def :refer-macros [defhtml]]
            [hiccup.page :refer [include-css]]
            [goog.dom :as dom]
            [clojure.string :refer [split join]]
            [cljs.reader :as reader :refer [read-string]])
  (:require-macros [ewen.ddom.core :refer [defnx]])
  (:import [goog.string format]))

(defprotocol IXNamed
  "Protocol for retrieving name of an exported def"
  (^string xname [x]
   "Returns the munged fully qualified name String of x."))

(defnx read-string-x [s]
  (read-string s))

(reader/register-tag-parser! 'ewen.ddom.core/exported
                             (fn [x]
                               (apply aget js/window (split x "."))))

(comment
  (read-string "#ewen.ddom.core/exported ewen.ddom.core.read_string_x")
  )

(defn handler [xfn & params]
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
