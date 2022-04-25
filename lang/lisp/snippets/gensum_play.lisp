(defmacro repeat (times &body body)
  (let ((x (gensym)))
    `(dotimes (,x ,times)
       ,@body)))

;; ,@  means comma splice ->
; let v = (oh boy)
; `(zap ,@v ,v) -> (zap oh boy (oh boy))

(repeat 3 (print 2))

(defvar x 'Hi)

(repeat 3 (print x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro cube (n)
  `(* ,n ,n ,n))

; This doesn't work

(let ((n 1))
  (cube (incf n))) ; => 24

(macroexpand-1 '(cube (incf n)))

; The actual evaluated expr is (* 2 3 4)
; because incf will mute the state of n

(defmacro cube* (n)
  (let ((x (gensym)))
    `(let ((,x ,n))
       (* ,x ,x ,x))))

(let ((n 1))
  (cube* (incf n)))
; (* 2 2 2) => 8


;; TODO inline vs macro @ https://dept-info.labri.fr/~strandh/Teaching/MTP/Common/David-Lamkins/chapter20.html
