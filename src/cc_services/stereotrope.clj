(ns cc-services.stereotrope
  "Generates poetry using a master-metaphor."
  (:require [clojure.string :as str]
            [clojure.xml :as xml]
            [ring.util.codec :as c]))

(def url "http://ngrams.ucd.ie/metaphor-magnet-acl/")

(defn- poem-line-xml->map
  [xml]
  (let [text (str/trim (get-in xml [:content 0 :content 0]))
        alt  (get-in xml [:content 1 :content 0])]
    (if alt
      (let [[quality stereotype] (str/split (str/trim alt) #":")]
        {:text text :quality quality :stereotype stereotype})
      {:text text})))

(defn- poem-xml->map
  [xml]
  (let [content (:content xml)]
    {:title (str/trim (get-in (first content) [:content 0]))
     :lines (map poem-line-xml->map (rest content))}))

(defn poem
  "Produces a poem using master metaphor described by quality, stereotype and
  target."
  [quality stereotype target]
  {:pre [(every? string? [quality stereotype target])]}
  (let [src    (c/percent-encode (str quality ":" stereotype))
        url    (str url "p?xml=true&source=" src "&target=" target)]
    (poem-xml->map (xml/parse url))))

(defn print-poem
  "Helper function for printing poems produced by stereotrope."
  [{:keys [title lines]}]
  (println title)
  (println)
  (doseq [line lines :let [text (:text line)]]
    (println text)))
