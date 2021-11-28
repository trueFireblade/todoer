(ns todoer.core
  (:gen-class))

(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(def silent false)

(defn get-lines-from-file [file-name]
  (if (.exists (io/file file-name))
    (str/split (slurp file-name) #"\n")
    (do (if silent "" (binding [*out* *err*]
      (println (str "File \"" file-name "\" doesn't exist. Skipping it")))
     )(vector ""))))

(defn get-todo-from-lines [lines]
  (vector 
    (apply list lines) 
    (map (fn [arg](re-find #"TOD(O+)" arg)) lines)))

(defn get-todo-from-file [file-name]
  (get-todo-from-lines
    (get-lines-from-file file-name)))

(defn get-todo-priority [todo]
  (if 
    (nil? todo)
    todo
    (let [[txt prio]todo]
      (count prio))))

(defn map-todo-priority [file-name]
  (let [[lines todo](get-todo-from-file file-name)]
    (vector lines
            (map get-todo-priority todo))))

(defn zip-with-infos [todo-map file-name]
  (apply map vector
         (vector
           (take 
             (count (nth todo-map 0))
             (repeat file-name))
           (map inc 
                (range (count (nth todo-map 0))))
           (map
             (fn[a] (re-find #"\S.*" a))
             (nth todo-map 0))
           (nth todo-map 1))))

(defn filter-zip-todo-prioritys [file-names]
  (reverse(sort-by last (filter
                          (fn [touple]
                            (let [[filen line-number line amount] touple]
                              (some? amount)))
                          (reduce concat (doall (map
                                   (fn [a] (zip-with-infos 
                                             (map-todo-priority a)
                                             a))
                                   file-names)))))))

(def extra-spaces 7)

(defn print-results-helper [filtered-ziped-list]
  (def max-len
    (try (apply max
      (map
        (fn [printable-result]
          (let [[filen line txt prio] printable-result]
            (+ (count filen) (count (str line)))))
        filtered-ziped-list))
      (catch Exception e (println "No Todo's here!"))))
  (if silent "" (println (str 
                                   "File:Line"
                                   (str/join "" (repeat (- (+ max-len extra-spaces) 8) " "))
                                   "Urgency   Text")))
  (map
    (fn [input] 
      (let [[filen line txt prio] input]
        (println (str 
                   filen ":" line 
                   (str/join ""
                     (repeat (- (+ max-len extra-spaces 1) (+ (count filen) (count (str line)) 1)) " "))
                   prio 
                   (str/join "" 
                     (repeat (- 10 (count (str prio))) " "))
                   (let [[prevTodo afterTodo]
                         (str/split txt #"TODO+")]
                     (str prevTodo "TODO" afterTodo))))))
    filtered-ziped-list))

(defn print-with-header-results-helper [filtered-ziped-list]
    (dorun (print-results-helper filtered-ziped-list)))

(defn classFilter [file-list]
    (filter
      (fn [filen]
        (nil? (re-find #".class" filen)))
      file-list))

(defn directoryResolver [file-list] (
    map
      (fn [file]
       (if
         (not (.isDirectory (io/file file)))
         file 
         (directoryResolver
           (map
             (fn [a]
               (str file "/" a))
             (.list (io/file file))))))
       file-list))

(defn print-results [file-names]
  (print-with-header-results-helper
    (filter-zip-todo-prioritys (map (fn [filen] (str/replace filen #"\./" "")) (classFilter (flatten (directoryResolver file-names))))))
  (System/exit 0))

(defn print-help [] (
  (println "Help for todoer")
  (println "Usage [options] file1 file2...")
  (println "Options:")
  (println "  -h\tPrints this menu")
  (println "  -s\tSurpresses the header")
  (System/exit 0)))

(defn -main [& args]
  (if (nil? args) (print-help) "")
  (if (= "-h" (first args)) (print-help) "")
  (if (= "-s" (first args)) ((def silent true)(print-results (drop 1 args))) "")
  (print-results args))
