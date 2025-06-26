;Jimmy Le, 40316405
(ns weather
  (:require [clojure.string :as str]))

(declare main-menu handle-choice)

(defn parse-line
  "Parses a comma-separated line into a weather report map."
  [line]
  (let [[date location temp condition] (str/split line #",")]
    {:date date
     :location location
     :temperature (Integer/parseInt temp)
     :condition condition}))

(defn load-reports
  "Loads weather reports from a file.
  Please refer to https://clojure.org/guides/threading_macros for more details on ->>
  "
  [filename]
  (if (.exists (java.io.File. filename))
    (->> (slurp filename)
         str/split-lines
         (map parse-line)
         vec)
    []))

(defn convert-to-fahrenheit
  "Converts an entry's temperature into Fahrenheit"
  [report]
  (update report :temperature #(->  (+ (Math/round (/ (* % 9) 5.0)) 32))))

(defn convert-to-celsius
  "Converts an entry's temperature into Celsius"
  [report]
  (update report :temperature #(-> (Math/round (*  (/ (- % 32) 9.0) 5)))))


(defn pad-field [field space]
  "This function will pad text to make it centered"
  (let [pad-total (max 0 (- space (count (str field))))
        pad-left  (int (/ pad-total 2))
        pad-right (- pad-total pad-left)]

    (str (apply str (repeat pad-left " "))
         field
         (apply str (repeat pad-right " ")))))

(defn convert-to-integer
  "this function will attempt convert a user input into an integer and return false if it failed"
  [input]
  (try
    (Integer/parseInt input)
    (catch NumberFormatException _ nil)
    ))

(defn print-header
  "Prints the header of the table"
  []
  (println "--------------------------------------------------------")
  (println "    Date    |   Location    | Temperature  |  Condition ")
  (println "--------------------------------------------------------"))


(defn print-all-entry
  "Prints the date, location, temperature and condition of an entry"
  [report unit]
  (let [degree (if (= unit "1") "\u00B0C" "\u00B0F")]
    (println (str
              (pad-field (:date report) 12) "|"
              (pad-field (:location report) 15) "|"
              (pad-field (str (:temperature report) degree)  14) "|"
              (pad-field "" 4)
              (:condition report)))))

(defn print-transformation
  "Prints out the options for transforming temperature"
  []
  (println "Choose a transformation: ")
  (println "1. Convert temperatures to Celsius")
  (println "2. Convert temperatures to Fahrenheit")
  (println "Enter your choice (1-2): "))

(defn print-filter
  "this function will print the filter options"
  []
  (println "\nFilter by: ")
  (println "1. Condition")
  (println "2. Temperature range")
  (println "Enter your choice (1-2): "))

(defn filter-by-condition
  "This function will return a new list containing the matching condition"
  [reports chosenCondition]
  (filter #(= (:condition %) chosenCondition) reports)
  )

(defn filter-by-temperature
  "This function will return a new list containing the matching condition"
  [reports minTemp maxTemp]
  (filter #(and (>= (:temperature %) minTemp) (<= (:temperature %) maxTemp)) reports)
  )

(defn calculate-avg
  "this function will calculate the average temperatute of a given list"
  [reports]
  (if (empty? reports) 
    0
      (let [sumTemp (reduce + (map :temperature reports))]
        (if (= sumTemp 0)
          0
          (/ (double sumTemp) (count reports))
          ))
    )
  )
(defn calculate-max
  "this function will return the highest temperature from a list"
  [reports]
  (apply max (map :temperature reports))
  )

(defn calculate-min
  "this function will return the highest temperature from a list"
  [reports]
  (apply min (map :temperature reports)))



;; --------------------------------------------------------------------------------  Main Functions ---------------------------------------------
(defn view-weather-reports
  "Write code to display weather reports in a tabular format."
  [reports unit]
  (println "\nTotal weather reports: " (count reports))
  (print-header)
  (doseq [report reports] (print-all-entry report unit)))

(defn transform-weather-reports
  "Write code to apply user-selected transformation to the weather report collection."
  [reports unit newUnit]

  (cond
    (= unit newUnit) reports
    (= unit "1") (map convert-to-fahrenheit reports)
    (= unit "2") (map convert-to-celsius reports)
    :else nil))



(defn filter-weather-reports
  "Write code to filter reports based on condition or temperature range."
  [reports unit input]

  (cond
    (= input "1")
    (do
      (println "Enter Condition: ")
      (let [choiceCondition (read-line)]
        (filter-by-condition reports choiceCondition)))
    (= input "2")
    (do
      (println "Enter min temperature: ")
      (let [minTemp (convert-to-integer(read-line))]
        (println "Enter max temperature: ")
        (let [maxTemp (convert-to-integer (read-line))]
          (if (or (nil? minTemp) (nil? maxTemp))
            nil
            (filter-by-temperature reports minTemp maxTemp)))))
    :else nil))


(defn weather-statistics
  "Write code to prints stats: average, hottest, coldest, unique conditions."
  [reports unit]
  (let [degree (if (= unit "1") "\u00B0C" "\u00B0F")]
    (println "\nAverage temperature: " (calculate-avg reports) degree)

    (let [hottestDays (filter-by-temperature reports (calculate-max reports) (calculate-max reports))
          coldestDays (filter-by-temperature reports (calculate-min reports) (calculate-min reports))]
      (println "\nHottest day(s): ")
      (println "--------------------------------------------------------")
      (doseq [day hottestDays] (print-all-entry day unit))
      (println "\nColdest day(s): ")
      (println "--------------------------------------------------------")
      (doseq [day coldestDays] (print-all-entry day unit))
      (println "\nUnique Conditions: " (count (set(map :condition reports))))
      ))
      )


    

(defn save-reports
  "Write your code. Saving is optional"
  [reports])

(defn exit-program []
  (println "\nThank you for using the Weather Report System. Goodbye!")
  (System/exit 0))

(defn main-menu
  ([file]
   (main-menu file (load-reports file) "1"))
  ([file reports unit]
   (println "\n=== Weather Report System ===")
   (println "1. View Weather Reports")
   (println "2. Transform Weather Report")
   (println "3. Filter Weather Reports")
   (println "4. Weather Statistics")
   (println "5. Save and Exit")
   (print "Enter your choice (1-5): ")
   (flush)
   (let [choice (read-line)]
     (handle-choice choice reports file unit))))

(defn handle-choice [choice reports file unit]
  (case choice
    "1" (do (view-weather-reports reports unit)
            (main-menu file reports unit))
    "2" (do
          (print-transformation)
          (let [tempChoice (read-line)
                updated (transform-weather-reports reports unit tempChoice)]
            (if (nil? updated)
              (do
                (println "Invalid option. Try again.")
                (main-menu file reports unit))
              (do
                (println "\nTransformed Weather Reports:")
                (view-weather-reports updated tempChoice)))
            (main-menu file updated tempChoice)))
    "3" (do
          (print-filter)
          (let [filteredData (filter-weather-reports reports unit (read-line))]
            (if (nil? filteredData)
              (println "Invalid Input. Canceling operation...")
              (view-weather-reports filteredData unit)))
          (main-menu file reports unit))
    "4" (do (weather-statistics reports unit)
            

            (main-menu file reports unit))
    "5" (exit-program)
    (do (println "Invalid option. Try again.")
        (main-menu file reports unit))))

;; Entry point
(defn -main [& args]
  (let [file "weather_data.txt"]
    (main-menu file)))

(-main)