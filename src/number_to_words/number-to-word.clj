(ns number-to-words.core
 (:gen-class))

(def ones {:1 "One", :2 "Two", :3 "Three", :4 "Four", :5 "Five", :6 "Six", :7 "Seven", :8 "Eight", :9 "Nine"})
(def tens {:1 "Ten", :2 "Twenty", :3 "Thirty", :4 "Forty", :5 "Fifty", :6 "Sixty", :7 "Seventy", :8 "Eighty", :9 "Ninety"})
(def elevens {:1 "Eleven", :2 "Twelve", :3 "Thirteen", :4 "Fourteen", :5 "Fifteen", :6 "Sixteen", :7 "Seventeen", :8 "Eighteen", :9 "Nineteen"})

(defmulti convert-to-digit :position :default :else)

(defmethod convert-to-digit :one [digit]
 (-> digit :value keyword ones))

(defmethod convert-to-digit :ten [digit]
 (-> digit :value keyword tens))

(defmethod convert-to-digit :eleven [digit]
 (-> digit :value keyword elevens))

(defmethod convert-to-digit :hundred [digit]
 (.concat (.toString ((keyword (:value digit)) ones)) " Hundred"))

(defmethod convert-to-digit :else [digit]
 (str ""))

(defn build-pattern [n]
 (cond
  (and (= (:t n) "0") (= (:o n) "0")) [:num :zero :zero]
  (and (= (:t n) "1") (= (:o n) "0")) [:num :one :zero]
  (= (:t n) "1") [:num :one :num]
  (= (:t n) "0") [:num :zero :num]))
 
(defmulti build-number-string build-pattern :default :else)

(defmethod build-number-string [:num :zero :zero] [n]
 (convert-to-digit {:position :hundred :value (:h n)}))

(defmethod build-number-string [:num :one :zero] [n]
 (str
  (convert-to-digit {:position :hundred :value (:h n)}) " and "
  (convert-to-digit {:position :ten :value (:t n)})))

(defmethod build-number-string [:num :one :num] [n]
(str
 (convert-to-digit {:position :hundred :value (:h n)}) " and "
 (convert-to-digit {:position :eleven :value (:o n)})))

(defmethod build-number-string [:num :zero :num] [n]
 (str
  (convert-to-digit {:position :hundred :value (:h n)}) " and "
  (convert-to-digit {:position :one :value (:o n)})))

(defmethod build-number-string :else [n]
 (str
  (convert-to-digit {:position :hundred :value (:h n)}) " and " 
  (convert-to-digit {:position :ten :value (:t n)}) " "
  (convert-to-digit {:position :one :value (:o n)})))

(defn convert-to-words [number]
 (def s (str number))
 (let [n {:h (str (.charAt s 0)), :t (str (.charAt s 1)), :o (str (.charAt s 2))}]
  (build-number-string n))) 
