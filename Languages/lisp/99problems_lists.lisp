;; [5] Reverse a list

;; tail-recursive with accumulator

(defun rec-reverse (lst)
  (labels ((rev (lst acc)
             (if (endp lst) ;endp -> end of a list
                 acc
                 ;; -> move every head of old list to the head of acc
                 (rev (rest lst) (cons (first lst) acc)))))
    (rev lst '())))


;; loop/iterative macro
(defun loop-reverse (lst)
  (loop
    :with result ='()
    :for x :in lst
    :do (push x result)
    :finally (return result)))


;; test
(mapc (lambda (fun)
        (ASSERT (equal (mapcar fun '(() (1 2 3) (a b c))) '(nil (3 2 1) (c b a)))))
      (list (function loop-reverse)
            (function rec-reverse)))

;; [8] elimate dupliaced elements

(defun rec-compress (lst)
  (labels ((aux (item lst)
              (cond
                ((null lst) (list item))
                ((eql item (first lst))  (aux item (rest lst)))
                (t (cons item (aux (first lst) (rest lst)))))))
    (cond
      ((null lst))
      ((null (rest lst)) lst)
      (t (aux (first lst) (rest lst))))))

(rec-compress '(1 1 1 1 2 3 3 3 4))

;; [10] Run-length encoding

(defun rec-encode (lst)
  (labels ((aux (element count lst)
              (cond
                ((null lst)                          (list (list count element)))
                ((eql element (first lst))   (aux element (1+ count) (rest lst)))
                (t    (cons (list count element) (aux (first lst) 1 (rest lst))))
              )
          ))
  (if (null lst)
      '()
      (aux (first lst) 1 (rest lst))))
)

(rec-encode '(a a a a b b a a a a c c c c c))
