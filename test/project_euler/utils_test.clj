(ns project-euler.utils-test
  (:use clojure.test
        project-euler.utils
        project-euler.test-constants))

(deftest test-primes
  (is (= first-1000-primes (take 1000 (primes)))))

(deftest test-digits
  (is (= '(1 2 3) (digits 123)))
  (is (= '(1 2 3 4 5 6 7 8 9) (digits 123456789)))
  (is (= '(1 2 0) (digits 120)))
  (is (= '(0) (digits 0)))
  (is (= '(1 0) (digits 012))))

(deftest test-pandigital?
  (is (= true (pandigital? 51234)))
  (is (= false (pandigital? 51233)))
  (is (= false (pandigital? 5124)))
  (is (= true (pandigital? 1)))
  (is (= true (pandigital? 578249631))))

(deftest test-triangulars
  (is (= '(1 3 6 10 15 21 28 36 45 55)
         (take 10 (triangulars)))))

(deftest test-triangular?
  (is (nil? (->> (triangulars)
              (take 1000)
              (map triangular?)
              (some false?)))))

(deftest test-pentagonals
  (is (= '(1 5 12 22 35 51 70 92 117 145 176 210 247 287 330 376 425 477 532 590 651 715 782 852 925 1001) 
         (take 26 (pentagonals)))))

(deftest test-pentagonal?
  (is (nil? (->> (pentagonals)
              (take 1000)
              (map pentagonal?)
              (some false?)))))

(deftest test-hexagonals
  (is (= '(1 6 15 28 45 66 91 120 153 190 231 276 325 378 435 496 561 630 703 780 861 946)
         (take 22 (hexagonals)))))

(deftest test-hexagonal?
  (is (nil? (->> (hexagonals)
              (take 1000)
              (map hexagonal?)
              (some false?)))))

(deftest test-odd-composites
  (is (= '(9 15 21 25 27 33 35 39 45 49)
         (take 10 (odd-composites)))))

(deftest test-r-from-coll
  (is (= #{#{1 2} #{1 3} #{2 3}}
         (into #{} (map (partial into #{}) (r-from-coll 2 [1 2 3])))))
  (is (= 15504 (count (r-from-coll 5 (range 20))))))

(deftest test-count-r-from-n
  (is (= 2598960 (count-r-from-n 5 52))))

(deftest test-permutations
  (is (= #{[1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1]}
         (into #{} (permutations [1 2 3]))))
  (is (= [[1 2 2] [1 2 2] [2 1 2] [2 2 1] [2 1 2] [2 2 1]]
         (permutations [1 2 2])))
  (is (= (! 8) (count (permutations (range 8))))))