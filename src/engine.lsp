(in-package alex-shell)

(defun startEngine()
  (format t "lmao")
)




;; helper functions to simplify condition expressions in knowledge base files
(defun xor(a b)
  (or (and a (not b)) (and b (not a))) 
)

(defun xand (a b)
  (not (xor a b))
)
