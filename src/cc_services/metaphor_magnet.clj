(ns cc-services.metaphor-magnet
  "Find and exploit commonplace metaphors in everyday texts and use these
  mappings to interpret more novel metaphors.

  Details on how to specify metaphors can be found:
  http://ngrams.ucd.ie/downloads/Metaphor%20Magnet%20README.pdf"
  (:require [clojure.string :as str]
            [clojure.xml :as xml]
            [ring.util.codec :as c]))

(def url "http://ngrams.ucd.ie/metaphor-magnet-acl/")

(defn- source-target-xml->map
  "Takes XML Source or Target entity returned by Metaphor-magnet and decodes it
  as a single map."
  [xml]
  (let [[property stereotype] (-> (get-in xml [:content 0 :content 0])
                                  (str/trim)
                                  (str/split #":"))]
    {:concept    ({:Source :source :Target :target} (:tag xml))
     :name       (get-in xml [:attrs :Name])
     :score      (Float/parseFloat (get-in xml [:content 1 :content 0]))
     :stereotype stereotype
     :property   property}))

(defn interpret
  "Takes a string containing a metaphor and returns a sequence of salient
  stereotypes and apt properties that can associated with it."
  [metaphor]
  {:pre [(string? metaphor)]}
  (let [metaphor (c/percent-encode metaphor)
        url      (str url "q?xml=true&kw=" metaphor)]
    (->> (xml/parse url)
         :content
         (map source-target-xml->map))))
