(ns vertere.venice
  (:require [vertere.mess :as vt] :reload))

(use '[cemerick.pomegranate :only (add-dependencies)])

;(add-dependencies :coordinates '[[org.clojure/tools.namespace "0.2.8"]]
;                  :repositories (merge cemerick.pomegranate.aether/maven-central
;{"clojars" "http://clojars.org/repo"}))

(require '[clojure.repl :as repl :refer [doc]])
(require '[net.cgrand.enlive-html :as html])
(require '[clojure.pprint :as pprint])
(require '[clojure.string :as str])
(use '[clojure.data :only [diff]])
(use '[clojure.java.shell :only [sh]])
(use '[clojure.set :only [union intersection]])

(defn fetch-file [path] (html/html-resource (java.io.FileReader. path)))

(defn fetch-slurp [path] (html/html-resource (java.io.StringReader. (slurp path))))

(def venice (fetch-file "venice.html"))

(-> (clojure.data/diff venice
                    vt/hamlet)
     (nth 2))

(vt/get-play venice)

(vt/get-play vt/hamlet)


(def ven (-> (vt/get-play venice)
             (vt/process-play)))

(->> (html/select ven
              [[:scene]])
     (map :attrs)
     pprint/pprint)

(html/select ven [[:scene (html/attr= :index 1)] :line])


(defn speaker-freqs-interest [play speaker]
  (->> (vt/speaker-frequencies play speaker)
       (remove #(vt/boring-words (key %)))
       (sort-by second)
       (map (fn [[w n]] [(symbol w) n]))
       reverse))
(doc sort-by)


(defn speaker-freqs-syms [play speaker]
  (->> (vt/speaker-frequencies play speaker)
       (remove #(vt/boring-words (key %)))
       (map (fn [[w n]] [(symbol w) n]))
       (into {})))

(->> (vt/speaker-frequencies ven 'launcelot)
     (remove #(vt/boring-words (key %)))
     (sort-by second)
     (reverse))

(speaker-freqs-interest ven :antonio)

(vt/speaker-word ven :antonio 'heart)

(pprint/pprint (vt/compare-freqs (speaker-freqs-syms ven :shylock) (speaker-freqs-syms ven :antonio)))

(vt/word-speakers ven "three")
(vt/word-coords ven 'belmont)

(vt/speaker-word ven "LAUNCELOT" "jew")



(vt/word-speakers ven 'why)

(vt/word-speakers ven "christian")



(intersection (->> (vt/speaker-lines ven "BASSANIO")
                   (map first)
                   (map #(take 2 %))
                   (set))
              (->> (vt/speaker-lines ven "ANTONIO")
                   (map first)
                   (map #(take 2 %))
                   (set)))

(vt/speaker-lines (vt/section-of ven :act 1 :scene 1) "ANTONIO")

(vt/word-coords ven "will")

