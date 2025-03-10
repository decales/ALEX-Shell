(defpackage :input
  (:use :cl)
  (:export :readInput)
)


(load "parse.lsp")


(defun readInput()
  (parseInput)
)
