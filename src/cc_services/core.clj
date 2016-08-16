(ns cc-services.core
  (:require [clojure.string :as str]
            [clojure.xml :as xml]
            [ring.util.codec :as codec]))

;;;; TODO: does URL mentioned in README still work?
;;; Does it drop XML type upon redirection?
;;; http://boundinanutshell.com/metaphor-magent
(def metaphor-magnet-url "http://ngrams.ucd.ie/metaphor-magnet-acl/")

(defn metaphor-xml->map
  "Takes XML Source or Target entity returned by Metaphor-magnet and encodes it
  as a single map."
  [xml]
  (let [concept              ({:Source :source :Target :target} (:tag xml))
        name                 (get-in xml [:attrs :Name])
        [quality stereotype] (-> (get-in xml [:content 0 :content 0])
                                 (str/trim)
                                 (str/split #":"))
        score                (Float/parseFloat (get-in xml [:content 1 :content 0]))]
    {:concept    concept
     :name       name
     :score      score
     :stereotype stereotype
     :quality    quality}))

(defn metaphor-magnet
  "Takes a string containing a metaphor and returns a sequence of salient
  stereotypes and apt qualities that can associated with it."
  [metaphor]
  {:pre [(string? metaphor)]}
  (let [metaphor (codec/percent-encode metaphor)
        url      (str metaphor-magnet-url "q?xml=true&kw=" metaphor)]
    (->> (xml/parse url)
         :content
         (map metaphor-xml->map))))

(def poetry-query "p?xml=true&")

(defn poem-line-xml->map
  [xml]
  (let [text (str/trim (get-in xml [:content 0 :content 0]))
        alt  (get-in xml [:content 1 :content 0])]
    (if alt
      (let [[quality stereotype] (str/split (str/trim alt) #":")]
        {:text text :quality quality :stereotype stereotype})
      {:text text})))

(defn poem-xml->map
  [xml]
  (let [content (:content xml)]
    {:title (str/trim (get-in (first content) [:content 0]))
     :lines (map poem-line-xml->map (rest content))}))

(defn stereotrope-poetry-generator
  [quality stereotype target]
  {:pre [(every? string? [quality stereotype target])]}
  (let [src    (codec/percent-encode (str quality ":" stereotype))
        url    (str metaphor-magnet-url "p?xml=true&source=" src "&target=" target)]
    (poem-xml->map (xml/parse url))))

(def therex-url "http://ngrams.ucd.ie/therex2/common-nouns/share.action?xml=true&")

(defn therex-member-xml->map
  [xml]
  {:item   (get-in xml [:content 0])
   :weight (get-in xml [:attrs :weight])})

(defn therex-members-xml->map
  [xml]
  (let [word1 (get-in xml [:attrs :word1])
        word2 (get-in xml [:attrs :word2])]
    (map therex-member-xml->map (:content xml)))

(defn theasaurus-rex
  [word1 word2]
  {:pre [(every? string? [word1 word2])]}
  (let [url (str therex-url "word1=" word1 "&word2=" word2)]
    (xml/parse url)
    ))
