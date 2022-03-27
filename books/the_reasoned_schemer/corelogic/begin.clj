(use 'clojure.core.logic)

(run* [q]
  (== q true))

(run* [q]
  (membero q [1 2 3])
  (membero q [2 3 4]))
