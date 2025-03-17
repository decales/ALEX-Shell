(defpackage :parse
  (:use :cl)
  (:export :parseExpressions)
)

(load "solve.lsp")

;; knowledge-base parameters
(defvar stateDefinition nil)
(defvar initialState nil)
(defvar goalState nil)
(defvar productionRules nil)

;; parse-specific variables
(defvar typeSpecifiers '(boolean bit string character integer float))
(defvar errorsDetected nil)


(defun parseExpressions(read-expressions)
  (setf errorsDetected nil)
  
  ;; check if file contains four expressions in the order they are expected
  (if (= 4 (length read-expressions))
    (progn
      (parseStateDefinition (nth 0 read-expressions))       ;; expression #1 - state definition property declarations
      (unless errorsDetected 
        (parseInitialState (nth 1 read-expressions))        ;; expression #2 - initial state values
        (parseGoalState (nth 2 read-expressions))           ;; expression #3 - goal state values
        (parseProductionRules (nth 3 read-expressions))     ;; expression #4 - list of production rules
      )
    )
    (parseError nil "expected 4 list parameters in the knowledge-base file but found ~d" (length read-expressions))
  )
  (unless errorsDetected
    (print "yep")
  )
)


(defun parseStateDefinition (read-stateDefinition)
  ;; check if state definition is formatted as list
  (if (typep read-stateDefinition 'list)
    ;; check if state definition list is not empty
    (if (< 0 (length read-stateDefinition))
      ;; check validity of each property
      (loop for property in read-stateDefinition do
        ;; check if property is formatted as list
        (if (typep property 'list)
          ;; check if property list contains exactly two expressions
          (if (= 2 (length property))
            (let ((name (nth 0 property)) (type (nth 1 property)))
              ;; check if first expression is a non-boolean symbol
              (if (and (typep name 'symbol) (not (typep name 'boolean)))
                ;; check if a property with the same name doesn't already exist
                (if (not (boundp name))
                  ;; create local variable for property
                  (eval `(defvar ,name nil))
                  (parseError read-stateDefinition "state definition: the name ~a can only be used to declare a single property" (string name))
                )
                (parseError property "state definition: first expression of property must be symbol")
              )
              ;; check if second expression is a data type specifier
              (let ((typeMatched))
                (loop for typeS in typeSpecifiers while (not typeMatched) do
                  (when (eq typeS type)
                    (setf typeMatched t)
                  )
                )
                (unless typeMatched
                  (parseError property "state definition: second expression of property must be a type-specifier symbol ~s" `',typeSpecifiers)
                )
              )
            )
            (parseError property 
              "state definition: expected two symbol expressions (<property-name> <property-type>) in property but found ~d" (length property))
          )
          (parseError property "state definition: property must be a list in the form (<property-name> <property-type>)")
        )
      )
      (parseError read-initialState "state definition list must not be empty")
    )
    (parseError read-initialState "state definition must be a list in the form ((<property-name> <property-type>) ... )")
  )
  (setf stateDefinition read-stateDefinition)
)


(defun parseInitialState (read-initialState)
  ;; check if initial state is formatted as list
  (if (typep read-initialState 'list)
    ;; check validity of each property
    (loop for property in read-initialState do
      ;; check if property is formatted as list
      (if (typep property 'list)
        ;; check if property list contains exactly two expressions
        (if (= 2 (length property))
          (let ((name (nth 0 property)) (value (nth 1 property))  (type (nth 1 (assoc (nth 0 property) stateDefinition))))
            ;; check if first expression is a non-boolean symbol
            (if (and (typep name 'symbol) (not (typep name 'boolean)))
              ;; check if first expression is a symbol of a name of a property is the state definition
              (if (boundp name)
                ;; check if property has not already been assigned a value
                (if (not (assoc name initialState))
                  ;; check if second expression is a value that matches the expected data type of the given property
                  (if (isType value type)
                    (progn ;; set value of local property variable to initial value - used to test production rule expressions
                      (eval `(setf ,name ,value))
                      (setf initialState (cons property initialState))
                    )
                    (parseError property "initial state: expected ~a expression value for ~a property" (string type) (string name))
                  )
                  (parseError read-initialState "initial state: the value for property ~a must only be set once" (string name))
                )
                (parseError property "initial state: no property with the name ~a is declared in the state definition" (string name))
              )
              (parseError property "initial state: property name must be symbol for the name of a property declared in the state definition")
            )
          )
          (parseError property "initial state: expected two expressions (<property-name> <property-value>) in property but found ~d" (length property))
        )
        (parseError property "initial state: property must be a list in the form (<property-name> <property-value>)")
      )
    )
    (parseError read-initialState "initial state: initial state must be a list in the form ((<property-name> <property-value>) ... )")
  )
  ;; check if initial state values have been set for all properties in the state definition
  (let ((missingProperties (getMissingProperties initialState)))   
    (when missingProperties
      (parseError nil "initial state: expected ~d properties declared in state definition - missing ~a" (length stateDefinition) `',missingProperties)
    )
  )
)


(defun parseGoalState (read-goalState)
  ;; check if goal state is formatted as list
  (if (typep read-goalState 'list)
    ;; check validity of each property
    (loop for property in read-goalState do
      ;; check if property is formatted as list
      (if (typep property 'list)
        ;; check if property list contains exactly two expressions
        (if (= 2 (length property))
          (let ((name (nth 0 property)) (value (nth 1 property))  (type (nth 1 (assoc (nth 0 property) stateDefinition))))
            ;; check if first expression is a non-boolean symbol
            (if (and (typep name 'symbol) (not (typep name 'boolean)))
              ;; check if first expression is a symbol of a name of a property is the state definition
              (if (boundp name)
                ;; check if the property has not already been assigned a value
                (if (not (assoc name goalState))
                  ;; check if second expression is a value that matches the expected data type of the given property
                  (if (isType value type)
                    ;; set value of local property variable to goal value - used to test production rule expressions
                    (setf goalState (cons property goalState))
                    (parseError property "goal state: expected ~a expression value for ~a property" (string type) (string name))
                  )
                  (parseError read-goalState "goal state: the value for property ~a must only be set once" (string name))
                )
                (parseError property "goal state: no property with the name ~a is declared in the state definition" (string name))
              )
              (parseError property "goal state: property name must be symbol for the name of a property declared in the state definition")
            )
          )
          (parseError property "goal state: expected two expressions (<property-name> <property-value>) in property but found ~d" (length property))
        )
        (parseError property "goal state: property must be a list in the form (<property-name> <property-value>)")
      )
    )
    (parseError read-goalState "goal state: goal state must be a list in the form ((<property-name> <property-value>) ... )")
  )
  ;; check if goal state values have been set for all properties in the state definition
  (let ((missingProperties (getMissingProperties goalState)))   
    (when missingProperties
      (parseError nil "goal state: expected ~d properties declared in state definition - missing ~a" (length stateDefinition) `',missingProperties)
    )
  )
)


(defun parseProductionRules (read-productionRules)
  ;; check if production rules is a list
  (if (typep read-productionRules 'list)
    ;; check if production rules list is not empty
    (if (< 0 (length read-productionRules))
      ;; check validity of each production, ensure logic of expressions align with the properties of the state definition
      (loop for i below (length read-productionRules) do
        (parseProduction (nth i read-productionRules) (+ 1 i))
      )
      (parseError nil "production rules list must not be empty")
    )
    (parseError read-productionRules
      "production rules must be a list in the form ((<condition-semantics> <condition> <action-semantics> <action>) ... )")
  )
)


(defun parseProduction (production index)
  ;; check if production is a list
  (if (typep production 'list)
    ;; check if production contains four expected parameters that evaluate to their expected types
    (if (= 4 (length production))
      (let ((condition-semantics (nth 0 production))
            (condition (nth 1 production)) 
            (action-semantics (nth 2 production)) 
            (action (nth 3 production)))

        ;; check condition semantics
        (unless (isType condition-semantics 'string)
          (parseError production "production rules: condition semantics of production #~d must be a string expression" index)
        )
        ;; check condition
        (unless (isType condition 'boolean)
          (parseError production "production rules: condition of production #~d must be a boolean expression" index)
        )
        ;; check action semantics
        (unless (isType action-semantics 'string)
          (parseError production "production rules: action semantics of production #~d must be a string expression" index)
        )
        ;; check action
        (if (typep action 'list)
          (loop for property in action do
            ;; check if property is a list
            (if (typep property 'list)
              ;; check if property list has exactly two expression
              (if (= 2 (length property))
                (let ((name (nth 0 property)) (value (nth 1 property))  (type (nth 1 (assoc (nth 0 property) stateDefinition))))
                  ;; check if first expression is a non-boolean symbol
                  (if (and (typep name 'symbol) (not (typep name 'boolean)))
                    ;; check if first expression is a symbol of a name of a property is the state definition
                    (if (boundp name)
                      ;; check if second expression is a value that matches the expected data type of the given property
                      (unless (isType value type)
                        (parseError property "production rules: expected ~a expression value for ~a property" (string type) (string name))
                      )
                      (parseError property "production rules: no property with the name ~a is declared in the state definition" (string name))
                    )
                    (parseError property "production rules: property name must be symbol for the name of a property declared in the state definition")
                  )
                )
                (parseError action 
                  "production rules: expected two expressions (<property-name> <new-property-value>) in property but found ~d" (length propertyUpdate))
              )
              (parseError action "production rules: property update must be a list in the form (<property-name> <new-property-value>)")
            )
          )
          (parseError production "production rules: action must be list in the form ((<property-name> <new-property-value>) ... )")
        )
      )
      (parseError production "production rules: expected 4 parameters in production #~d but found ~d" index (length production))
    )
    (parseError property "production rules: production must be a list in the form (<condition-semantics> <condition> <action-semantics> <action>)")
  )
)


(defun isType (expression type)
  ;; try to evaluate the expression, then check if evaluation matches expected data type
  (handler-case (typep (eval expression) type) (error (e) (typep expression type)))
)


(defun getMissingProperties (target)
  (let ((missingProperties nil))
    (loop for p1 in (mapcar #'car stateDefinition) do
      (let ((isFound))
        (loop for p2 in (mapcar #'car target) while (not isFound) do (setf isFound (eq p1 p2)))
        (unless isFound (setf missingProperties (cons p1 missingProperties)))
      )
    )
    (return-from getMissingProperties missingProperties)
  )
)


(defun parseError (expression message &rest vars)
  (format t "error - ~s~%" (eval `(format nil ,message ,@vars)))
  (when expression (format t "input - ~s~%~%" expression))
  (setq errorsDetected t)
)
