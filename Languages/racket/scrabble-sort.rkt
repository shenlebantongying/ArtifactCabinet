(define (lookup x)
  (second (assoc x '(("IT" 3) ("OF" 5) ("CAN" 7) ("DEAN" 5) ("JEST" 9) ("ONE" 3)))))

(define (insert x lst)
  (cond
    [(empty? lst) (list x)]
    [(<= (lookup x) (lookup (first lst))) (cons x lst)]
    [else (cons (first lst) (insert x (rest lst)))]))

(define (scrabble-sort lst)
  (cond
    [(empty? lst) empty]
    [else (insert (first lst) (scrabble-sort (rest lst)))]))

(scrabble-sort (list "IT" "OF" "CAN" "DEAN" "JEST" "ONE"))
