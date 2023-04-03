;; emulating "break"
(setq x 0 total 0)
(catch 'break
  (while t
    (incf total x) ; => INCrement Function quoted -> in-place increment
    (if (> (incf x) 10)
        (throw 'break total))))

;; emulating "continue"
(setq x 0 total 0)
(while (< x 100)
  (catch 'continue
    (incf x)
    (if (zerop (% x 5))
        (throw 'continue nil))
    (incf total x)))

;; -> java
;; var x = total = 0;
;; while (x < 100) {
;;   x++;
;;   if (x % 5 == 0) {
;;      continue;
;;   }
;;    total += x;
;; }

;; combine break and continue
(setq x 0 total 0)
(catch 'break
  (while t
    (catch 'continue
      (incf x)
      (if (>= x 100)
          (throw 'break nil))
      (if (zerop (% x 5))
          (throw 'continue nil))
      (incf total x))))
