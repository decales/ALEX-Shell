(load "input.lsp")


(defun printShell ()
  (format t "fexsh>> ")
)


(defun printAbout()
  (format t "    __               _
   / _|             | |    
  | |_ _____  _____ | |__  
  |  _/ _ \\ \\/ / __|| '_ \\ 
  | ||  __/>  <\\__ \\| | | |
  |_| \\___/_/\\_\\___/|_| |_|
  ")
  (format t "~%Welcome to the farmer-problem expert system shell~%")
  (format t "Created by Declan Urbaniak-Dornstauder for COMP 452~%~%")
)


(defun printCommands()
   (format t "List of available commands:~%")
   (format t "  'quit' or 'q' ------- terminate the current session~%")
   (format t "  'help' or 'h' ------- print information about this shell and the list of valid commands~%")
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

  (let ((command (nth 0 tokens)))
    (when (not (string= "" command))
      (cond
        ;; quit shell
        ((or (string= command "quit") (string= command "q"))
         (format t "goodbye :)~%" command)
         (setq shellRunning nil)
        )
        ;; print help
        ((or (string= command "help") (string= command "h"))
         (printCommands)
        )
        ;; input 
        ((or (string= command "input") (string= command "i"))

         (let ((filepath (nth 1 tokens)))
           (if filepath
             (readInput filepath)
             (format t "error - 'input' must be followed by the path of the file to be read~%")
            )
          )
        )
        (t ;; default - command not recognized
          (format t "~s is not a valid command - use 'help' or 'h' for a list of valid commands~%" command)
        )
      )
    )
  )
)


(defvar shellRunning t)

(printAbout)
(printCommands)

(loop while shellRunning do
  (printShell)
  (let ((tokens (getTokens)))
    (getCommand tokens)
  )
)
