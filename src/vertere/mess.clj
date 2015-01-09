(ns vertere.mess)

;(use '[cemerick.pomegranate :only (add-dependencies)])

;; (add-dependencies :coordinates '[[org.clojure/math.combinatorics "0.0.8"]]
;;                   :repositories (merge cemerick.pomegranate.aether/maven-central
;;                                        {"clojars" "http://clojars.org/repo"}))

(require '[clojure.repl :as repl :refer [doc]])
(require '[net.cgrand.enlive-html :as html])
(require '[clojure.pprint :as pprint])
(require '[clojure.string :as str])
(use '[clojure.data :only [diff]])
(use '[clojure.java.shell :only [sh]])
(use '[clojure.set :only [union]])


(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(def measure (fetch-url "http://shakespeare.mit.edu/measure/full.html"))
(def hamlet (fetch-url "http://shakespeare.mit.edu/hamlet/full.html"))
(def merchant (fetch-url "http://shakespeare.mit.edu/hamlet/full.html"))
measure

(defn get-play [res]
  (html/select res
               #{[:body :> :h3]
                 [:body :> :a]
                 [:body :> :blockquote]}))

(defn chunkinator [tag content1-regex content]
  (partition-by
   #(not (and (= (:tag %) tag)     ;; because if both are truthy, `and` returns last value,
              ;; but partition-by is interested in equality, not truthiness, so we take the complement
              (re-find content1-regex (first (:content %)))))
   content))

(defn chunkprocessor [tag continuation]
  (fn [i [chunk-marker chunk-content]]
    {:tag tag
     :attrs {:index (inc i)
             :heading (first (:content (first chunk-marker)))}
     :content (continuation chunk-content)}))

(def testvar :ooga)

(defn parse-coords [coords]
  (vec (map #(Integer/parseInt %)
            (str/split coords #"\."))))

(defn process-scene [scene-content]
  (let [scene {:tag :scene :content scene-content}
        chunked (html/at scene
                         [:scene :> :blockquote :> :i] (fn [sd] {:tag :stage-direction
                                                                 :content (:content sd)})
                         [:scene :> :a] (fn [dm] {:tag :dialog
                                                  :attrs {:speech-designator (-> dm :attrs :name)
                                                          :speaker (-> dm :content first :content first)}
                                                  :content '()})
                         [:scene :> :blockquote :> :p :> :i] (fn [sd] {:tag :stage-direction
                                                                       :content (:content sd)})
                         [:scene :> :blockquote :> :a] (fn [ln] {:tag :line
                                                                 :attrs {:line-coordinates (parse-coords (-> ln :attrs :name))}
                                                                 :content (map str/triml (:content ln))})
                         [:blockquote] (fn [bq] (assoc bq :content (remove #(= % "\n") (:content bq))))
                         ; [:br] (constantly nil)
                         {[:dialog] [:blockquote]} (fn [[dlg bq]] (assoc dlg :content (:content bq)))
                         [:scene :> :blockquote]   (fn [bq] (first (:content bq))))]
    (:content (first chunked))))


(defn process-act [act-content]
  (let [scenes-raw (chunkinator :h3 #"SCENE [IVX]+" act-content)
        scenes (map-indexed (chunkprocessor :scene process-scene)
                            (partition 2 scenes-raw))]
    scenes))

(defn process-play [play]
  (let [acts-raw (chunkinator :h3 #"ACT [IV]+" play)
        acts (map-indexed (chunkprocessor :act process-act)
                          (partition 2 acts-raw))]
    acts))

(def m4m (-> (get-play measure)
             (process-play)))

(def ham (-> (get-play hamlet)
             (process-play)))

(def ven (-> (get-play merchant)
             (process-play)))

(def boring-words
  (into #{}
        (map
         #(.toLowerCase %)
         (str/split-lines
          (slurp "englishwords/all.txt")))))

(defn speaker-lines [p speaker]
  (let [speaker (.toUpperCase (name speaker))]
    (->> (html/select p [[:dialog (html/attr= :speaker speaker)] :line])
         (map (fn [x] [(-> x :attrs :line-coordinates)
                       (-> x :content first)])))))

(defn speaker-frequencies [p speaker]
  (let [speaker (.toUpperCase (name speaker))]
    (->> (str/split (apply str
                           (map second (speaker-lines p speaker)))
                    #"[^a-zA-Z'\-]")
         (remove #{""})
         (map #(.toLowerCase %))
         (frequencies))))

(defn speaker-word [p speaker word]
  (let [word (name word)
        speaker (.toUpperCase (name speaker))]
    (map #(identity [(-> % :attrs :line-coordinates)
                     (-> % :content first)])
         (filter
          (fn [x]
            (some #{(.toLowerCase word)} (map #(.toLowerCase %)
                                              (-> x :content first (str/split #"[^a-zA-Z'\-]")))))
          (html/select p [[:dialog (html/attr= :speaker speaker)] :line])))))

(defn compare-freqs [freqs1 freqs2]
  (->> (merge-with vector freqs1 freqs2)
       (filter #(vector? (val %)))
       (remove #(boring-words (key %)))
       (remove #(apply = (val %)))
       (sort-by #(apply / (val %)))))

(defn print-comp-freqs [comp-freqs]
  (->> comp-freqs
       (map (fn [[word [i a]]] (str a "a " i "i " word)))
       (interpose \newline)
       (apply println)))

(defn word-speakers [p word]
  (let [word (name word)]
    (->> (html/select p [:dialog])
         (map (fn [dlg]
                (when (some #{(.toLowerCase word)}
                            (map #(.toLowerCase %)
                                 (str/split
                                  (apply str
                                         (map #(-> % :content first)
                                              (html/select dlg [:line])))
                                  #"[^a-zA-Z'\-]")))
                  (-> dlg :attrs :speaker))))
         (set)
         (keep identity))))

(defn word-coords [p word]
  (let [word (.toLowerCase (name word))]
    (->> (html/select p [:dialog])
         (filter (fn [dlg]
                   (some #{word}
                         (str/split
                          (.toLowerCase
                           (apply str
                                  (map #(-> % :content first)
                                       (html/select dlg [:line]))))
                          #"[^a-zA-Z'\-]"))))
         (map (fn [dlg]
                [(-> dlg :attrs :speaker)
                 (filter #(some #{word}
                                (str/split (-> % :content first .toLowerCase) #"[^a-zA-Z'\-]"))
                         (html/select dlg [:line]))]))
         (mapcat (fn [[sp lns]]
                   (map (fn [ln]
                          [sp (-> ln :attrs :line-coordinates) (:content ln)])
                        lns))))))

(defn section-of [p & specs]
  (html/select p (vec (for [[t i] (partition 2 specs)
                            :when i]
                        [t (html/attr= :index i)]))))

(pprint/pprint (word-coords m4m "poor"))
(pprint/pprint (word-coords m4m "own"))

;; TODO
;; how to select scenes with characters.
;; how to operate on parts of nodes transparently without losing
;; the rest of the information of the node.
;; separate out stage-directions at ends of speeches
;; fix the weird run-together-words problem


(def claudio-freqs (speaker-frequencies m4m "CLAUDIO"))
(def isabella-freqs (speaker-frequencies m4m "ISABELLA"))
(def angelo-freqs (speaker-frequencies m4m "ANGELO"))
(def lucio-freqs (speaker-frequencies m4m "LUCIO"))
(def escalus-freqs (speaker-frequencies m4m "ESCALUS"))
(def pompey-freqs (speaker-frequencies m4m "POMPEY"))
(def vincentio-freqs (speaker-frequencies m4m "DUKE VINCENTIO"))
(count lucio-freqs)
(count isabella-freqs)

lucio-freqs
(speaker-word m4m "DUKE VINCENTIO" "poor")

(speaker-word m4m "ISABELLA" "poor")

(speaker-word m4m "POMPEY" "poor")

(speaker-word m4m "LUCIO" "poor")

(reverse (sort-by val (remove #(boring-words (key %))
                              isabella-freqs)))

(compare-freqs angelo-freqs isabella-freqs)
(compare-freqs vincentio-freqs isabella-freqs)
(compare-freqs vincentio-freqs angelo-freqs)


(word-speakers m4m "poor")

(speaker-word m4m "ISABELLA" "poor")

(speaker-lines m4m "ELBOW")

(reverse (sort-by second (remove #(boring-words (key %)) (speaker-frequencies ham "HAMLET"))))

(speaker-word ham "HAMLET" "good")


(pprint/pprint (word-coords (section-of m4m :act 3 :scene 1) "poor"))

(pprint/pprint(word-coords ham "poor"))

(->> (map (partial speaker-frequencies m4m) ["CLAUDIO" "DUKE VINCENTIO" "ISABELLA"
                                             "ANGELO" "LUCIO" "POMPEY" "ESCALUS"])
     (apply merge-with +)
     (remove #(boring-words (key %)))
     (sort-by val)
     (reverse))

(pprint/pprint (speaker-lines m4m "ABHORSON"))


