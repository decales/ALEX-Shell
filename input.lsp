(defpackage :input
  (:use :cl)
  (:export :readInput)
)


(load "parse.lsp")


(defun readInput(filepath)
  ;; check if the file exists
  (if (probe-file filepath)
    ;; read each expression from the inputted file into a list then pass list of expressions to parser 
    (parseExpressions
      (with-open-file (stream filepath :direction :input)
        (loop for expression = (read stream nil 'eof) until (eq expression 'eof) collect expression)
      )
    )
    (format t "error - the file ~s does not exist or could not read~%" filepath)
  )
)
