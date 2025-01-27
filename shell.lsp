
(defun getTokens()
  (setq tokens nil)
  (let ((inputStr (read-line)))
    (setq j 0)
    (loop for i from 0 below (length inputStr) do
      (when (char= #\space (char inputStr i))
        (setq tokens (cons (subseq inputStr j i) tokens))
        (setq j (+ i 1))
      )
    )
    (setq tokens (reverse (cons (subseq inputStr j (length inputStr)) tokens)))
  )
)


(defvar shellRunning t)

(loop while shellRunning do

  (format t "fexsh>>")
  (let ((tokens (getTokens)))
    (print tokens)
  )
)
