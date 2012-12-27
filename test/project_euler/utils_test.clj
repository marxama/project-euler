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
