(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all ])
  (:gen-class ))

(defn is-link [element]
  (and (vector? element) (= (first element) :a )))

(defn parse-link [r-class-tag]
  (:href (first (filter map? (first (filter is-link r-class-tag))))))


(defn dfs [data]
  (if (= "r" (:class (first (filter map? data))))
    (parse-link data)
    (reduce (fn [result-vector value]
              (if (vector? value)
                (into result-vector value)
                (conj result-vector value)))
      [] (map dfs (filter vector? data)))))


(defn get-links []
  " 1) Find all elements containing {:class \"r\"}.

  Example:
  [:h3 {:class \"r\"} [:a {:shape \"rect\", :class \"l\",
                           :href \"https://github.com/clojure/clojure\",
                           :onmousedown \"return rwt(this,'','','','4','AFQjCNFlSngH8Q4cB8TMqb710dD6ZkDSJg','','0CFYQFjAD','','',event)\"}
                       [:em {} \"clojure\"] \"/\" [:em {} \"clojure\"] \" · GitHub\"]]

     2) Extract href from the element :a.

  The link from the example above is 'https://github.com/clojure/clojure'.

    3) Return vector of all 10 links.

  Example: ['https://github.com/clojure/clojure', 'http://clojure.com/', . . .]
  "
  (let [data (parse "clojure_google.html")]
    (dfs data)))

(defn -main []
  (println (str "Found " (get-links) " links!")))


