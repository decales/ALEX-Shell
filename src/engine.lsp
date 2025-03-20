(in-package alex-shell)

(defun startEngine()
  (solveDFS `((,initialState nil nil)))
)


;; solve problem instance using depth first search
;; nodes as lists (<state> <condition-semantics> <action-semantics>)
(defun solveDFS (visitedNodes)
  ;; retrieve the current state from the top of the visited stack
  (let ((state (car (car visitedNodes))))

    ;; check if the current state is goal state
    (when (stateEquals state goalState)
      (print "done")
      (return-from solveDFS)
    )

    ;; macro to declare state properties as local variables
    ;; (letStateProperties state
    ;;   ;; try match state/knowledge to each production rule
    ;;   (loop for production in productionRules do
    ;;     (let ((condition-semantics (nth 0 production))
    ;;           (condition (nth 1 production))
    ;;           (action-semantics (nth 2 production))
    ;;           (action (nth 3 production)))
    ;;
    ;;       ;; check if a production fires in the current state
    ;;       (print condition)
    ;;       (if (eval `,condition) ;; evaluates using locally scoped property variables 
    ;;         (let ((updatedState (getUpdatedState state action))) ;; get updated state given the action of the production
    ;;
    ;;           (print updatedState)
    ;;
    ;;           ;; check if the updated state is unvisited,
    ;;           (unless (isVisited (mapcar #'car visitedNodes) updatedState)
    ;;             ;; create node with updated state and condition/action semantics, then add it to the visited stack and traverse to it
    ;;             (solveDFS (cons `(,updatedState ,condition-semantics ,action-semantics) (copy-tree visitedNodes)))
    ;;           )
    ;;         )
    ;;         (format t "no ")
    ;;       )
    ;;     )
    ;;   )
    ;; )
  )
)


;; iterates through a list of states to check if it contains a target state
(defun isVisited(visitedStates state)
  (loop for vState in visitedStates do
    (when (stateEquals vState state)
      (return-from isVisited t)
    )
  )
  (return-from isVisited nil)
)


(defun stateEquals(state1 state2)
  (loop for property in state1 do
    (let ((name (nth 0 property)) (value1 (nth 1 property))  (value2 (nth 1 (assoc (nth 0 property) state2))))
      ;; check if any properties values of the two states don't match 
      (unless (equal value1 value2)
        (return-from stateEquals nil)
      )
    )
  )
  ;; if all property values match, states are equal
  (return-from stateEquals t)
)


(defun getUpdatedState(state action)
  ;; apply each property update from the action to the state
  (let ((updatedState (copy-tree state)))
    (loop for property in action do
      (let ((name (nth 0 property)) (value (nth 1 property)))
        (setf (nth 1 (assoc name updatedState)) value)
      )
    )
    (return-from getUpdatedState updatedState)
  )
)


;; helper functions to simplify condition expressions in knowledge base files
(defun xor (p q)
  (or (and p (not q)) (and q (not p)))
)


(defun xand (&rest args)
  (loop for i below (- (length args) 1) do
    (let ((p (nth i args)) (q (nth (+ 1 i) args)))
      (when (xor p q)
        (return-from xand nil)
      )
    )
  )
  (return-from xand t)
)

