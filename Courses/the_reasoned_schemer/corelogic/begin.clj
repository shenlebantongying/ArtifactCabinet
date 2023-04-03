;; see Core.logic Primer

;; TODO: this bring everything into the namespace, avoid this?
(use 'clojure.core.logic)

(run* [q]
  (== q true))

(run* [q]
  (membero q [1 2 3])
  (membero q [2 3 4]))

(run* [q a b c d e]
  (== q (range 10))
  (matche [ q ]
          [ [[. a b c d e]] ]))

;; ([(0 1 2 3 4 5 6 7 8 9) 0 1 2 3 (4 5 6 7 8 9)])


(run* [q]
     (matche [q]
      ([ [a . b] ])))

;; _0 , _1 means anything and they can be distant from each other.
;; _0 , _0 means anything but they have to be the same


(run* [q]
  (fresh [a]
    (membero a [2 3])
    (== a q))) ;; => (2 3)

;; a is a member of [2 3]
;; a and q are unified.
;; Note [a] won't occur on result.

;; conde just quit after one succeed match

(run* [q]
  (conda
   [(membero 1 [q 2 3])]
   [(membero q [4 5 6])])) ;; => q -> 1


(run* [q]
  (conda
   [(membero q [4 5 6])]
   [(membero 1 [q 2 3])])) ;; => q -> [4,5,6]
