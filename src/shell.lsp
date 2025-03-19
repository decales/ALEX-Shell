(in-package alex-shell)

;; global shell variables
(defvar shellRunning t)

(defun msgPrompt()
  (format t "~%(alexsh)$ ")
)


(defun msgAbout()
  (msg nil nil
  "     _   _    _____  __
    /_\\ | |  | __\\ \\/ /
   / _ \\| |__| _| >  < 
  /_/ \\_\\____|___/_/\\_\\
  ")         
  (msg 'info nil "Welcome to \"a LISP expert-system\" shell - created by Declan Urbaniak-Dornstauder for COMP 452")
)


(defun msgCommands()
  (msg 'info nil "list of commands")
  (msg nil nil 
     "~2T'quit' or 'q' -------------------------- terminate the current session~
     ~%~2T'help' or 'h' -------------------------- print information about this shell and the list of commands~
     ~%~2T'input <file>' or 'i <file>' ----------- input and load a knowledge-base file~
     ~%~2T'start' or 's' ------------------------- start the inference engine with the current knowledge-base~
     ~%~2T'file' or 'f' -------------------------- view the properties of the current knowledge-base"
  )
)


(defun msgExpectedFormat(errorMessage)
  (msg 'error nil errorMessage)
  (msg nil nil 
     "~2Teg. ((<property-name> <property-type>) (...) )   ;; parameter #1 - state definition~
     ~%~6T((<property-name> <property-value>) (...) )  ;; parameter #2 - initial state~
     ~%~6T((<property-name> <property-value>) (...) )  ;; parameter #3 - goal state~
     ~%~6T(                                            ;; parameter #4 - production rules~
     ~%~7T(<condition-semantics>~
     ~%~8T<condition>~
     ~%~8T<action-semantics>~
     ~%~8T<action>)~%~
     ~%~7T(...)~
     ~%~6T)"
  )
)


(defun msg(leader input str &rest vars)
  (if leader
    (format t "(~a): ~a~%" (string-downcase leader) (eval `(format nil ,str ,@vars)))
    (format t "~a~%" (eval `(format nil ,str ,@vars)))
  )
  (when input (format t "~2Tinput: ~s~%~%" input))
  (setf errorsDetected t)
)


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


(defun getCommand(tokens)
  (let ((command (string-downcase (nth 0 tokens))))
    (when (not (string= "" command))
      (cond
        ;; quit shell
        ((or (string= command "quit") (string= command "q"))
          (msg 'info nil "goodbye :)")
          (setq shellRunning nil)
        )
        ;; print help
        ((or (string= command "help") (string= command "h"))
          (msgCommands)
        )
        ;; input knowledge-base 
        ((or (string= command "input") (string= command "i"))
          (let ((file (nth 1 tokens)))
            ;; check if a path to a file was included in the command
            (if file
              ;; check if knowledge-base is already loaded
              (if knowledgeBaseLoaded
                (progn
                  (msg 'warning nil "a knowledge-base file is already loaded - do you want to override this file with the current input? (y/n) ")
                  (msgPrompt)
                  (let ((response (read-line)))
                    (cond
                      ((or (string= response "y") (string= response "yes") (string= response "igen")) 
                        (readFile file)
                      )
                      ((or (string= response "n") (string= response "no") (string= response "nem"))
                        (msg 'info nil "aborting current knowledge-base input"))
                      (t
                        (msg 'error nil "'~a' is not a valid response - aborting current knowledge-base input" response)
                      )
                    )
                  )
                )
                ;; otherwise this is a new session, read the file
                (readFile file)
              )
              (msg 'error nil "'input' must be followed by the path of the knowledge-base file to be read")
            )
          )
        )
        ;; start engine
        ((or (string= command "start") (string= command "s"))
          ;; check if knowledge-base is loaded
          (if knowledgeBaseLoaded
            (startEngine)
            (msg 'error nil "a knowledge-base file is not loaded - use 'input <file>' to input and load a knowledge-base file" command)
          )
        )
        ;; knowledge-base file information
        ((or (string= command "file") (string= command "f"))
          (if knowledgeBaseLoaded
            (msg 'info nil "didn't feel like implementing this command (but trust me, it's all there)")
            
            (msg 'error nil "a knowledge-base file is not loaded - use 'input <file>' to input and load a knowledge-base file" command)
          )
        )
        (t ;; default - command not recognized
          (msg 'error nil "'~a' is not a valid command - use 'help' or 'h' for a list of valid commands" command)
        )
      )
    )
  )
)


(defun startShell()
  (readFile "fp.lsp")
  (when knowledgeBaseLoaded (startEngine))

  ;; (msgAbout)
  ;; (msgCommands)
  ;;
  ;; (loop while shellRunning do
  ;;   (msgPrompt)
  ;;   (let ((tokens (getTokens)))
  ;;     (getCommand tokens)
  ;;   )
  ;; )
)
