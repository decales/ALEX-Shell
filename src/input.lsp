(in-package alex-shell)

(defvar expressions nil)

(defun readFile(file)
  (setf expressions nil)

  ;; check if the file exists
  (if (probe-file file)
    (handler-case
      ;; read each expression from the inputted file into a list then pass list of expressions to parser 
      (with-open-file (stream file :direction :input)
        (setf expressions (loop for expression = (read stream nil 'eof) until (eq expression 'eof) collect expression))
      )
      (error (e)
        (msgExpectedFormat (format nil 
          "the file '~a' could not be read - please ensure the expected parameters are syntactically correct and in the correct order" file))
      )
    )
    (msg 'error nil "the file '~a' does not exist" file)
  )
  ;; check if file contains four expressions in the order they are expected
  (when expressions
    (if (= 4 (length expressions))
      (parseExpressions expressions)
      (msgExpectedFormat (format nil 
        "expected 4 parameters in the file but found ~d - please ensure only the expected parameters are present and in the correct order" 
        (length expressions)))
    )
  )
)
