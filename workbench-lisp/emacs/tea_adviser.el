;;;; Minimum emacs interactive buffer

; More complete example -> doctor.el on emacs source tree

(define-derived-mode tea-mode text-mode "Tea Adviser")

(defvar responses
  '("A" "B" "C"))

(defun tea-repl ()
  (interactive)
  (setq adviser--get (tea-readin))
  (insert "\n\n")
  (insert ">")
  (insert (elt responses (random (length responses))))
  (insert "\n\n")
)

(defun tea-readin ()
  "Read user input, return the whole line"
  (let (sentence)
    (setq sentence (buffer-substring (line-beginning-position) (line-end-position)))
    sentence))

;; TODO: autoload?

;;;###autoload
(defun tea ()
  (interactive)
  (define-key tea-mode-map "\r" 'tea-repl)
  (pop-to-buffer-same-window "*tea-advicer*")
  (switch-to-buffer "*doctor*")
  (insert "Hello, this is your tea master!\n\n")
  (tea-mode)
)

; -> M-x function
(provide 'tea)
