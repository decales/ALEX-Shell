;; Customizable struct to represent a state in the context of the problem to be solved
;; You can declare as many state properties as you like and name them as you please
(defstruct state
  farmer
  wolf
  goat
  cabbage
)

;; Initial state
;; You must define all properties of the custom state struct
(defvar initial
  (make-state
    :farmer t
    :wolf t
    :goat t
    :cabbage t
  )
)

;; Goal state
;; You must define all properties of the custom state struct
(defvar goal
  (make-state
    :farmer nil
    :wolf nil
    :goat nil
    :cabbage nil
  )
)

;; List of production rules (condition-action pairs)
;; - Production rules are represented as instances of the non-customizable struct 'production-rule'
;; - You can define as many production rules as you like by inserting a 'make-production-rule' block in the 'production-rules' list below
;; - Each 'production-rule' instance should have the following properties:
;;    - condition-semantics:  string that semantically describes the logic of the 'condition' in the context of the problem
;;    - condition:            lambda function containing boolean expression to represent the constraints of an 'action'
;;    - action-semantics:     string that semantically describes the function of the 'action' in the context of the problem
;;    - action:               lambda function containing expression to update the "knowledge" of the program or the current state
;; - 'condition' and 'action' must take the form of a lambda function with one parameter 's' of type 'state' to represent the current state
;; - Therefore, the expression inside a lambda function can access the custom properties of 's' as defined in the 'state' struct in this file
;; - 'condition' expressions must evaluate to a boolean representing whether the current state 's' has met the conditions to fire an action
;; - 'action' expressions must evaluate to a 'state' instance that represents the new state after an action has occured in the current state 's'
;; - In general, it is your responsibilty to ensure that an expression...
;;    - consists of valid Lisp syntax
;;    - aligns with the data types and names of the properties defined in the custom state struct 
;;    - is correct in the context of the problem to be solved
;;    - does not compromise the safety and security of your system (due to its ambiguity, and whether intentional or not)
(defvar production-rules

  (make-production-rule
    :condition-semantics "wolf and goat are not together and goat and cabbage are not together"
    :condition (lambda (s) (and (not (xand (state-wolf s) (state-goat s))) (not (xand (state-goat s) (state-cabbage s)))))
    :action-semantics "farmer travels alone to opposite shore"
    :action (lambda (s) (setf (state-farmer s) (not (state-farmer s))))
  )

  ;; ... add rest of production-rules later
)



