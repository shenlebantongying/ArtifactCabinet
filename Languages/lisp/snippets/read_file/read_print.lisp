(defun read-by-line (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(read-by-line "nice.csv")
