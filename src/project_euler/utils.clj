(ns project-euler.utils)

(defn fibonnacci
  "Returns an infinite seq of terms in the Fibonnacci sequence."
  []
  (->> (iterate (fn [[a b]] [b (+ a b)]) [1 2])
    (map first)))

(defn divisable?
  "Returns whether or not num is evenly divisible by div."
  [num div]
  (zero? (rem num div)))

(defn prime?
  [x]
  (cond 
    (< x 2) false
    (= x 2) true
    :else (not (some #(divisable? x %) (->> x
                                         Math/sqrt
                                         Math/ceil
                                         inc
                                         (range 2))))))

(defn primes
  "Returns an infinite seq of all primes."
  []
  (cons 2N (->> (iterate (partial + 2) 3N)
            (filter prime?))))

(defn reverse-number
  [x]
  (->> x
    str
    reverse
    (drop-while (partial = \0))
    (apply str)
    read-string))

(defn palindrome?
  "Returns whether or not x is a palindromic number. For example, 121 is
   palindromic, whereas 122 is not." 
  [x]
  (= x (reverse-number x)))

(defn !
  [x]
  (if (zero? x) 1
    (apply * (map bigint (range 1 (inc x))))))

(defn r-from-coll
  "Returns a seq of all combinations of choosing r items from coll"
  [r coll]
  (let [f 
        (fn f [r coll subcoll]
          (if 
            (= r (count subcoll)) subcoll
            (apply concat
                   (map-indexed 
                     (fn [i c]
                       (f r (drop (inc i) coll) (conj subcoll c))) 
                     coll))))]
    (partition r (f r coll []))))

(defn count-r-from-n
  "Returns the number of combinations of taking r items from a set of n."
  [r n]
  (/ (! n)
     (* (! r) (! (- n r)))))