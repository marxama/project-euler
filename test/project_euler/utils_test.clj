(ns project-euler.utils-test
  (:use clojure.test
        project-euler.utils
        project-euler.test-constants))

(deftest test-primes
  (is (= first-1000-primes (take 1000 (primes)))))
