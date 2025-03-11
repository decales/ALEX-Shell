(defpackage :input
  (:use :cl)
  (:export :readInput)
)


(load "parse.lsp")


(defun readInput(filepath)
  ;; check if the file exists
  (if (probe-file filepath)
    (progn
      (with-open-file (stream filepath :direction :input)
        (loop while (setq line (read-line stream nil)) do
          (parseInput line)
        )
      )
    )
    (format t "error - the file ~s does not exist or could not read~%" filepath)
  )
)
