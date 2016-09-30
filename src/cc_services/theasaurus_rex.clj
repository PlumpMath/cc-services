(ns cc-services.theasaurus-rex
  "Thesaurus Rex organizes words according to the fine-grained ad-hoc categories
  they are placed into by speakers in everyday language.

  It reports categories for a single word or shared categories for pairs of
  words harvested from the web.

  Salience is measured by a weight parameter.

  Similar to WordNet but more useful when a divergent view of conceptual
  structure is required."
  (:require [clojure.string :as str]
            [clojure.xml :as xml]))

(defn- category-xml->map
  [xml]
  (let [[nuance category] (-> (get-in xml [:content 0])
                              (str/trim)
                              (str/split #":"))]
    {:category category
     :nuance   nuance
     :weight   (Double/parseDouble (get-in xml [:attrs :weight]))}))

(defn- modifier-xml->map
  [xml]
  {:modifier (str/trim (get-in xml [:content 0]))
   :weight   (Double/parseDouble (get-in xml [:attrs :weight]))})

(defn- category-head-xml->map
  [xml]
  {:category-head (str/trim (get-in xml [:content 0]))
   :weight        (Double/parseDouble (get-in xml [:attrs :weight]))})

(defn- member-data-xml->map
  [xml]
  {:categories     (map category-xml->map (get-in xml [:content 0 :content]))
   :modifiers      (map modifier-xml->map (get-in xml [:content 1 :content]))
   :category-heads (map category-head-xml->map (get-in xml [:content 2 :content]))})

(defn categories
  "Report categories word is found in."
  ([word] (categories word true))
  ([word disambiguate?]
   {:pre [(string? word) (instance? Boolean disambiguate?)]}
   (let [url (str "http://ngrams.ucd.ie/therex2/common-nouns/member.action?xml=true"
                  "&needDisamb=" disambiguate?
                  "&member=" word "&kw=" word)]
     (-> (clojure.xml/parse url)
         (member-data-xml->map)
         (assoc :word word)))))

(defn- members-xml->map
  [xml]
  (let [word1 (get-in xml [:attrs :word1])
        word2 (get-in xml [:attrs :word2])]
    {:word1 (get-in xml [:content 0 :attrs :word1])
     :word2 (get-in xml [:content 0 :attrs :word2])
     :shared-categories (map member-data-xml->map (get-in xml [:content 0 :content]))
     :for1-from2 (map category-xml->map (get-in xml [:content 1 :content]))
     :for2-from1 (map category-xml->map (get-in xml [:content 2 :content]))}))

(defn shared-categories
  "Report shared categories for two words."
  [word1 word2] {:pre [(every? string? [word1 word2])]}
  (let [url (str "http://ngrams.ucd.ie/therex2/common-nouns/share.action?xml=true&"
                 "word1=" word1 "&word2=" word2)]
    (members-xml->map (clojure.xml/parse url))))
