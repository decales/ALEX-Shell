(load "input.lsp")


(defun printShell ()
  (format t "alexsh>> ")
)


(defun printAbout()

  (format t "     _   _    _____  __
    /_\\ | |  | __\\ \\/ /
   / _ \\| |__| _| >  < 
  /_/ \\_\\____|___/_/\\_\\
  ")         

  (format t "~%Welcome to 'a LISP expert-system' shell~%")
  (format t "Created by Declan Urbaniak-Dornstauder for COMP 452~%~%")
)


(defun printCommands()
   (format t "List of commands:~%")
   (format t "  'quit' or 'q' -------------------------- terminate the current session~%")
   (format t "  'help' or 'h' -------------------------- print information about this shell and the list of commands~%")
   (format t "  'input <file>' or 'i <file>' ----------- input and load a knowledge-base file~%")
   (format t "  'start' or 's' ------------------------- print all solutions given the current knowledge-base~%")
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
        ;; input knowledge-base 
        ((or (string= command "input") (string= command "i"))
         (let ((filepath (nth 1 tokens)))
           (if filepath
             (readInput filepath)
             (format t "error - 'input' must be followed by the path of the knowledge-base file to be read~%")
            )
          )
        )
        ;; solve problem
        ((or (string= command "start") (string= command "s"))
         (if knowledgeBaseRead
           ()
           (format t "error - no knowledge-base is currently loaded - use 'input <file>' to input and load a knowledge-base file~%" command)
         )
        )
        (t ;; default - command not recognized
          (format t "error - ~s is not a valid command - use 'help' or 'h' for a list of valid commands~%" command)
        )
      )
    )
  )
)


(defvar shellRunning t)
(defvar knowledgeBaseRead nil)

(printAbout)
(printCommands)

(loop while shellRunning do
  (printShell)
  (let ((tokens (getTokens)))
    (getCommand tokens)
  )
)
