;; constructors
(defun make-leaf (E) (list E))
(defun make-node (E B1 B2) (list E B1 B2))

;; selectors
(defun tree-node (N)  (first N))
(defun tree-left (N) (second N))
(defun tree-right (N) (third N))

;; recongizers/predicts

(defun tree-leaf-p (B)
  (and (listp B)
       (= (list-length B) 1)))

(defun tree-node-p (B)
  (and (listp B))
  (= (list-length B) 3))

;; Test

(defvar slbT
  (make-node '*
           (make-node '+
                      (make-leaf 1)
                      (make-leaf 2))
           (make-node '-
                      (make-leaf 3)
                      (make-leaf 4))))

;; => (* (+ (1) (2)) (- (3) (4)))

;; Searching

(defun tree-member-p (B E)
  "Test if E is an element of tree B"
  (if (tree-leaf-p B)
      ;; then
      (equal E (tree-node B))
      ;; else
      (or (equal E (tree-node-p B))
          (tree-member-p (tree-left  B) E)
          (tree-member-p (tree-right B) E))))

(tree-member-p slbt 1)

;; TODO: reverse a tree

;; https://www2.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/3/tutorial3.html
