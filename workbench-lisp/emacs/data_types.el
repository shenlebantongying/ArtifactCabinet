(number-sequence 1 10 3)

;; alist -> association list
(setq x
      '(("A" . 1)
        ("B" . 2)
        ("C" . 3)
        ))

;; get by key
(cdr (assoc "A" x))

;; get by value
(car (rassoc 2 x))
