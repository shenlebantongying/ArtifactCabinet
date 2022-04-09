(ns slb.plg
  (:require slb.test))

(slb.test/nice "Called from plg")

(apply #(+ %1 %2) `(1 2))

(let [a-vect [1 2 3 4]
      a-map  {:name "nice"}]
  (list
   (conj a-vect 5)
   (assoc a-map :id "312")))

;; varadic funciton
(defn v-hi [greeting & who]
  (println greeting who))

(v-hi "hi" "tom" "john") ;; => hi (tom john)

;; data structures

;; => Collections in clj are all immutable and equality by value

(get [1 2 3 "4"] 3)
(get (vector 1 2 3 "4") 3)

(def names #{"emily","alice","kay"})
(contains? names "kay")

(str "3 > 2? " (if (> 3 2) "yes" "no"))

(defn mapper [x]
  (case x
    1 "x is 1"
    2 "x is 2"
    3 "x is 3"))

(mapper 2)
(#'user/mapper 3)

;; for i in range(10)
;; starts at zero, < 10
(doseq [n (range 10)]
  (println n))

;; multiple binding

(for ;; list comprehension
 [a (range 3)
  b (range 5)]
  [a b])

(doseq [a (range 3)
        b (range 5)]
  (prn [a b]))

;; [0 0]
;; [0 1]
;; [0 2]
;; [0 3]
;; [0 4]
;; [1 0]
;; [1 1]
;; [1 2]
;; [1 3]
;; [1 4]
;; [2 0]
;; [2 1]
;; [2 2]
;; [2 3]
;; [2 4]

;; Recrusive sum
;; not good
(defn sum [x]
  (if (= (count x) 0)
    0
    (+ (first x) (sum (rest x)))))

(sum [1 2 3])

