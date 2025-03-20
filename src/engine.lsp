(in-package alex-shell)


(defvar solutions nil)


(defun startEngine()
  ;;solve problem using depth first state search, then print solutions if any
  (setf solutions nil)
  (solveDFS `((,initialState nil nil)))
  (msgSolutions)
)

;; solve problem instance using depth first search
;; nodes represented as lists (<state> <condition-semantics> <action-semantics>)
(defun solveDFS (visitedNodes)
  
  ;; retrieve the current state from the top of the visited stack
  (let ((state (car (car visitedNodes))))

    ;; check if the current state is goal state
    (when (stateEquals state goalState)
      ;; add solution path to list of solutions
      (setf solutions (cons visitedNodes solutions))
      (return-from solveDFS)
    )
    ;; in non-solution state, try match state/knowledge to each production rule
    (loop for production in productionRules do
      (let ((condition-semantics (nth 0 production))
            (condition (nth 1 production))
            (action-semantics (nth 2 production))
            (action (nth 3 production)))

        ;; create dynamically scoped variables for each property of the current state, then match them to production rules
        ;; vars are dynamic in that they are bound in the let block scope, but 'special' declaration allows them to be used with (eval) outside of it
        ;; this language is simulatenously crazy and amazing, but this was a nightmare to figure out
        (eval `(let ,state 
          (declare (special ,@(mapcar #'first state)))

          ;; check if rule fires - conditions are expanded and evaluated using the dynamic variables
          (when ,condition  
            ;; get updated state given the action of the production
            (let ((updatedState (getUpdatedState ',state ',action))) 
              ;; ;; check if the updated state is unvisited,
              (unless (isVisited (mapcar #'car ',visitedNodes) updatedState)
                ;; create node with updated state and condition/action semantics, then add it to the visited stack and traverse to it
                (solveDFS (cons (list updatedState ,condition-semantics ,action-semantics) ',visitedNodes))
              )
            )
          )
        ))
      )
    )
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
      (let ((name (nth 0 property)) (value (getValue (nth 1 property)))) ;; getValue from parse.lsp
        (setf (nth 1 (assoc name updatedState)) value)
      )
    )
    (return-from getUpdatedState updatedState)
  )
)


;; helper functions to simplify expressions in knowledge base files
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

