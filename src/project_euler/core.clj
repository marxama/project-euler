(ns project-euler.core
  (:require [project-euler.utils :as utils]))


(defn problem-1
"If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000."
  []
  (->> (range 1000)
    (filter #(or (utils/divisible? % 3)
                 (utils/divisible? % 5)))
    (apply +)))


(defn problem-2
"Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms."
  []
  (->> (utils/fibonnacci)
    (filter even?)
    (take-while #(< % 4000000))
    (apply +)))


(defn problem-3
"The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?"
  []
  (let [num 600851475143
        possible-primes (->> (utils/primes)
                          (take-while #(< % (-> num
                                              Math/sqrt
                                              Math/ceil
                                              long))))]
    (->> possible-primes
      reverse
      (filter #(zero? (rem num %)))
      first)))


(defn problem-4
"A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.

Find the largest palindrome made from the product of two 3-digit numbers."
  []
  (let [products (for [x (reverse (range 100 1000))
                       y (reverse (range x 1000))]
                   (* x y))
        palindromes (filter utils/palindrome? products)]
    (apply max palindromes)))


(defn problem-5
"2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

SOLUTION: Being smart about it, it's very straight forward. All primes from 2 to 20 will need to be part of
the product. In addition, some of the primes will need to be applied multiple times, since divisibility by 2
does not imply divisibility by 4 - however, divisibility by 2 * 2 and 3 does imply divisibility by both 4, 6
12, for example."
  []
  (* 2 2 2 2 3 3 5 7 11 13 17 19)
  #_(->> (range)
    (drop 1)
    (filter #(every? (partial utils/divisible? %) (range 1 21)))
    first))


(defn problem-6
"Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

IMPROVEMENTS: I just know there's a good formula for this..."
  []
  (let [sum-of-squares (->> (range 1 101)
                         (map #(* % %))
                         (apply +))
        square-of-sums (->> (range 1 101)
                         (apply +)
                         (#(* % %)))]
    (- square-of-sums sum-of-squares)))


(defn problem-7
"By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?"
  []
  (nth (utils/primes) 10000))


(def problem-8-input
  "73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450")

(defn problem-8
"Find the greatest product of five consecutive digits in the 1000-digit number."
  []
  (let [digits (->> problem-8-input
                 (remove #{\newline})
                 (apply str)
                 utils/digits)
        products (->> digits
                   (partition 5 1)
                   (remove #(some (partial = 0) %)) ;let's count sequences with 0 out
                   (map (partial apply *)))]
    (apply max products)))


(defn problem-9
"There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc."
  []
  (let [square #(* % %)]
    (->> (for [a (range 1 1000)
               b (range (inc a) 1000)]
           (let [c (- 1000 a b)]
             (if (= (+ (square a) (square b)) (square c))
               (* a b c)
               nil)))
      (remove nil?)
      first)))


(defn problem-10
"The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million."
  []
  (->> (utils/primes)
    (take-while #(< % 2000000))
    (apply +)))


(defn problem-29
  []
  (let [a (map bigint (range 2 101))
        b (map bigint (range 2 101))]
    (->> (for [a a
               b b]
           (apply * (repeat b a)))
      distinct
      count)))


(defn problem-33
"The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the value of the denominator."
  []
  (for [numerator (range 1 10)
        denominator (range numerator 10)
        common-digit (range 1 10)]
    (let [n1 (+ numerator (* 10 common-digit))
          n2 (+ (* 10 numerator) common-digit)
          d1 (+ denominator (* 10 common-digit))
          d2 (+ (* 10 denominator) common-digit)
          fractions [(/ n1 d1) (/ n2 d1) (/ n1 d2) (/ n2 d2)]]
      (->> fractions
        (filter #(< % 1))
        (filter )))))


(defn problem-37
"The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

IMPROVEMENTS: Do not convert between numbers and strings."
  []
  (let [truncations (fn [x rest-or-butlast]
                      (->> x
                        str
                        seq
                        (iterate #(rest-or-butlast %))
                        (take-while (complement empty?))
                        (map (partial apply str))
                        (map #(Long/parseLong %)))) 
        all-truncations (fn [x]
                          (concat (truncations x rest)
                                  (truncations x butlast)))
        truncatable? (fn [x]
                       (every? utils/prime? (all-truncations x)))
        truncatable-primes (->> (utils/primes)
                             (drop 4)
                             (filter truncatable?)
                             (take 11))]
    (apply + truncatable-primes)))


(defn problem-52
"It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits."
  []
  (letfn [(have-same-digits 
            [& xs]
            (->>
              (map (comp sort seq str) xs)
              (apply =)))
          (test-digit
            [x]
            (apply have-same-digits (map * (repeat x) (range 1 7))))]
         (->> (drop 1 (range))
           (drop-while (complement test-digit))
           first)))


(defn problem-53
  []
  (let [counts (apply concat
                      ; Remember that nCr = nC(n-r)
                      (for [n (range 1 101)
                            r (range 1 (-> (/ n 2)
                                         int
                                         inc))]
                        (let [count (utils/count-r-from-n r n)]
                          (if (= r (/ n 2))
                            [count]
                            [count count]))))]
    (->> counts
      (filter #(> % 1000000))
      count)))


(defn problem-55
"If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.

Not all numbers produce palindromes so quickly. For example,

349 + 943 = 1292,
1292 + 2921 = 4213
4213 + 3124 = 7337

That is, 349 took three iterations to arrive at a palindrome.

Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome. A number that never forms a palindrome through the reverse and add process is called a Lychrel number. Due to the theoretical nature of these numbers, and for the purpose of this problem, we shall assume that a number is Lychrel until proven otherwise. In addition you are given that for every number below ten-thousand, it will either (i) become a palindrome in less than fifty iterations, or, (ii) no one, with all the computing power that exists, has managed so far to map it to a palindrome. In fact, 10677 is the first number to be shown to require over fifty iterations before producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).

Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.

How many Lychrel numbers are there below ten-thousand?"
  []
  (letfn [(lychrel?
            [x]
            (->> (iterate #(+ % (utils/reverse-number %)) x)
              (drop 1)
              (take 49)
              (some utils/palindrome?)
              not))]
         (->> (range 1 10000)
           (map bigint)
           (filter lychrel?)
           count)))
