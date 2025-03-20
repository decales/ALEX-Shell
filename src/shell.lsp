(in-package alex-shell)

(defvar tokens nil)
(defvar shellRunning t)
(defvar lastFileInputted nil)
(defvar debuggingEnabled nil)


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
  (msg 'info nil "Welcome to \"a LISP expert-system\" shell - created by Declan Urbaniak-Dornstauder for COMP 456")
)


(defun msgCommands()
  (msg 'info nil "list of commands")
  (msg nil nil 
     "~2T'quit' or 'q' -------------------------- terminate the current session~
     ~%~2T'help' or 'h' -------------------------- print information about this shell and the list of commands~
     ~%~2T'load <file>' or 'l <file>' ------------ input and load a knowledge-base file~
     ~%~2T'reload' or 'r' ------------------------ reload the last knowledge-base file that was inputted~
     ~%~2T'info' or 'i' -------------------------- view the properties of the current knowledge-base~
     ~%~2T'start <depth>' or 's <depth>' --------- start the inference engine with the current knowledge-base and an optional max search depth cutoff~
     ~%~2T'debug' or 'd' ------------------------- enable or disable debugging messages"
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


(defun msgKnowledgeBaseInfo()
  (msg 'info nil "current knowledge base properties")
  (msg nil nil 
    "~%~2Tinput file: ~s~
    ~%~2Tstate definition: ~a~
    ~%~2Tinitial state: ~a~
    ~%~2Tgoal state: ~a~
    ~%~%~2Tproduction rules:" 
     lastFileInputted '`,stateDefinition (formatState initialState "=") (formatState goalState "=") 
  )
  (loop for i below (length productionRules) do
    (let ((condition-semantics (nth 0 (nth i productionRules)))
          (condition (nth 1 (nth i productionRules)))
          (action-semantics (nth 2 (nth i productionRules)))
          (action (nth 3 (nth i productionRules))))

      (msg nil nil "~%~4Tproduction #~d:" (+ 1 i))
      (msg nil nil
        "~6Tcondition semantics: ~s~
        ~%~6Tcondition: ~a~
        ~%~6Taction semantics: ~s~
        ~%~6Taction: ~a"
        condition-semantics `',condition action-semantics (formatState action ":=")
      )
    )
  )
)




(defun msgSolutions()
  (if solutions ;; from engine.lsp
    (progn
      (msg 'info nil "~d solution path(s) found" (length solutions))
      ;; print each solution
      (loop for i below (length solutions) do
        (let ((solution (nth i solutions)))
          (msg 'info nil "solution #~d - ~d steps" (+ 1 i) (length solution))

          ;; print out the solution path step by step using last state and production semantics
          (msg nil nil "~%~2Tinitial state: ~a~%" (formatState (nth 0 (nth 0 solution)) "="))
          (loop for j from 1 below (- (length solution) 1) do
            (let ((state  (formatState (nth 0 (nth j solution))"=")) 
                  (condition-semantics (nth 1 (nth j solution)))  
                  (action-semantics (nth 2 (nth j solution))))

              (msg nil nil "~2T~s  ==>  ~s~%~2Tresulting state: ~a~%" condition-semantics action-semantics state)
            )
          )
          (msg nil nil "~2T~s  ==>  ~s~%~2Tgoal state reached: ~a~%" 
            (nth 1 (car (last solution))) (nth 2 (car (last solution))) (formatState (nth 0 (car ( last solution))) "="))
        )
      )
    )
    (msg 'info nil "no solutions found")
  )
)


;; helper function to print the state properties out nicer
(defun formatState(state symbolStr)
  (with-output-to-string (s)
    (loop for (name value) in state for first = t then nil do 
      (unless first (format s ", ")) (format s "~a ~a ~a" name symbolStr value)
    )
  )
)


(defun msg(leader input str &rest vars)
  ;; don't print debug messages when debugging isn't enabled
  (when (and (equal leader 'debug) (not debuggingEnabled))
    (return-from msg)
  )
  (if leader
    (if input
      (format t "(~a): ~a~%^input^: ~s~%" (string-downcase leader) (eval `(format nil ,str ,@vars)) input)
      (format t "(~a): ~a~%" (string-downcase leader) (eval `(format nil ,str ,@vars)))
    )
    (if input
      (format t "~a~%^input^: ~s~%" (eval `(format nil ,str ,@vars)) input)
      (format t "~a~%" (eval `(format nil ,str ,@vars)))
    )
  )
  (when (equal leader 'error)
    (setf errorsDetected t)
  )
)


(defun getTokens()
  (setf tokens nil)
  (let ((inputStr (read-line)))
    (let ((j 0))
      (loop for i from 0 below (length inputStr) do
        (when (char= #\space (char inputStr i))
          (setf tokens (cons (subseq inputStr j i) tokens))
          (setf j (+ i 1))
        )
      )
      (setf tokens (reverse (cons (subseq inputStr j (length inputStr)) tokens)))
    )
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
        ((or (string= command "load") (string= command "l"))
          (let ((file (nth 1 tokens)))
            ;; check if a path to a file was included in the command
            (if file
              ;; check if knowledge-base is already loaded
              (if knowledgeBaseLoaded
                (progn
                  (msg 'warning nil "a knowledge-base is already loaded - do you want to override the knowledge-base with the current input? (y/n) ")
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
              (msg 'error nil "'load' must be followed by the path of the knowledge-base file to be read")
            )
          )
        )
        ((or (string= command "reload") (string= command "r"))
          (if lastFileInputted
            (progn
              (msg 'info nil "reloading ~s" lastFileInputted)
              (readFile lastFileInputted)
            )
            (msg 'error nil "a previous knowledge-base file was not inputted - use 'load <file>' to input and load a knowledge-base file" command)
          )
        )
        ;; knowledge-base file information
        ((or (string= command "info") (string= command "i"))
          (if knowledgeBaseLoaded
            (msgKnowledgeBaseInfo)
            (msg 'error nil "a knowledge-base file is not loaded - use 'load <file>' to input and load a knowledge-base file" command)
          )
        )
        ;; start engine
        ((or (string= command "start") (string= command "s"))
          ;; check if knowledge-base is loaded
          (if knowledgeBaseLoaded
            ;; check if depth cutoff is included
            (let ((maxDepth (nth 1 tokens)))
              (if maxDepth
                ;; check if the depth cutoff input is an integer
                (handler-case
                  (if (< 0 (parse-integer maxDepth))
                    (startEngine (parse-integer maxDepth))
                    (msg 'error nil "max depth parameter must be a positive integer - aborting engine initiation")
                  )
                  (error (e)
                    (msg 'error nil "max depth parameter must be a positive integer - aborting engine initiation")
                  )
                )
                (startEngine nil)
              )
            )
            (msg 'error nil "a knowledge-base file is not loaded - use 'load <file>' to input and load a knowledge-base file" command)
          )
        )
        ;; toggle debugging
        ((or (string= command "debug") (string= command "d"))
          (if debuggingEnabled
            (progn
              (msg 'info nil "debugging mode has been disabled")
              (setf debuggingEnabled nil)
            )
            (progn
              (msg 'info nil "debugging mode has been enabled")
              (setf debuggingEnabled t)
            )
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
  (msgAbout)
  (msgCommands)

  (loop while shellRunning do
    (msgPrompt)
    (let ((tokens (getTokens)))
      (getCommand tokens)
    )
  )
)
