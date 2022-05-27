(ns uritemplate-clj.codec
  "taking neccesary details from ring.util.codec to derive logic to clj(s) lambdaisland.uri.normalize"
  (:require
   [clojure.string :as str]
   [lambdaisland.uri.normalize :as norm])
  #?(:clj (:import [java.net URLEncoder])))

(defn- norm-url-encode
  "taken from lambdaisland.uri.normalize/percent-encode but adding char-class
  ... perhaps we can fork lambdaisland.uri lib to add the specific encode char-class and type"
  [s char-class]
 (let [encode-char #(cond-> % (re-find char-class %) norm/percent-encode)]
   (->> (norm/char-seq s)
        (map encode-char)
        (apply str))))

;; taken from ring.util.codec
(defn- double-escape [ x]
  (str/replace (str/replace x "\\" "\\\\") "$" "\\$"))

;; replacing (ring.util.codec/url-encode)
(defn url-encode
  ([s]
   (url-encode s #"[^A-Za-z0-9_~.+-]+"))
  ([s char-class]
   (str/replace
    s
    char-class
    #(double-escape
      (norm-url-encode % char-class)))))

;; replacing (ring.util.codec/form-encode)
(defn form-encode
  "we only need str support of ring.util.codec/form-encode so adding hardcoded (and cljs) too
  grabbing impl from no.en.core/url-encode
  "
  [s]
  (when s
    #?(:clj (-> (URLEncoder/encode (str s) "UTF-8")
                (str/replace "%7E" "~")
                (str/replace "*" "%2A")
                (str/replace "+" "%20"))
       :cljs (-> (js/encodeURIComponent (str s))
                 (str/replace "*" "%2A")))))
