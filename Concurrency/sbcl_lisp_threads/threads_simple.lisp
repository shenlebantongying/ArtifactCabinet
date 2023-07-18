(defpackage :mutex_demo (:use "CL" "SB-THREAD" "SB-EXT"))
(in-package :mutex_demo)

(defvar *a-mutex* (make-mutex :name "a lock for ya"))

(defun a-thread-with-mutex ()
  (with-mutex (*a-mutex*)
    (format t "Thread ~A get lock ~%" *current-thread*)
    (sleep 1)
    (format t "Thread ~A dropped lock ~%" *current-thread*))
  )

(defvar a (make-thread #'a-thread-with-mutex))
(defvar b (make-thread #'a-thread-with-mutex))
(defvar c (make-thread #'a-thread-with-mutex))

(join-thread a)
(join-thread b)
(join-thread c)


(format t "NO MUTEX== ~%")

;; FUNNY: format is not perfectly working with threads

(defun a-thread-without-mutex ()
  (sleep (random 5))
  (format t "Thread ~A ~%" *current-thread*))

(defvar d (make-thread #'a-thread-without-mutex))
(defvar e (make-thread #'a-thread-without-mutex))
(defvar f (make-thread #'a-thread-without-mutex))

(join-thread d)
(join-thread e)
(join-thread f)

