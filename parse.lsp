(defpackage :parse
  (:use :cl)
  (:export :readInput)
)


(load "solve.lsp")

(defstruct production-rule
  condition 
  action
)

(defun parseInput(line)


  (setq p0 (make-production-rule
      :condition nil
      :action nil
    )
  )


  (print p0)






)
