(in-package alex-shell)

;; knowledge-base parameters
(defparameter stateDefinition nil)
(defparameter initialState nil)
(defparameter goalState nil)
(defparameter productionRules nil)
(defparameter knowledgeBaseLoaded nil)

;; parse-specific variables
(defvar errorsDetected nil)
(defvar typeSpecifiers
  '(boolean bit string simple-string character char standard-char base-char integer fixnum bignum float short-float single-float double-float long-float))


(defun parseExpressions(read-expressions)
  (reset) ;; reset bound property variables and parameters on call

  
  (parseStateDefinition (nth 0 read-expressions))                   ;; expression #1 - state definition property declarations
  (parseStates (nth 1 read-expressions) (nth 2 read-expressions))   ;; expressions #2 and #3 - initial and goal state values
  (parseProductionRules (nth 3 read-expressions))                   ;; expression 4 - list of production rules

  ;; check if all parameters read without errors
  (if (and stateDefinition initialState goalState productionRules (not errorsDetected))
    (progn
      (setf knowledgeBaseLoaded t) ;; signal to shell module that inputted knowledge-base is valid
      (msg 'info nil "knowledge-base file read and loaded successfully")
      (msg 'info nil "use 'start' or 's' to start the inference engine with the provided knowledge-base")
      (msg 'info nil "use 'info' or 'i' to view the properties of the provided knowledge-base")
    )
    (msg 'info nil "aborting current knowledge-base input")
  )
)


(defun parseStateDefinition(read-stateDefinition)
  (msg 'debug nil "parsing state definition...")
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
            (let ((name (nth 0 property)) (type (nth 1 property)) (nameValid) (typeValid))
              ;; check if first expression is a non-boolean symbol
              (if (and (typep name 'symbol) (not (typep name 'boolean)))
                ;; check if a property with the same name doesn't already exist
                (if (not (assoc name stateDefinition))
                  (setf nameValid t)
                  (msg 'error read-stateDefinition "state definition - the name ~a can only be used to declare a single property" (string name))
                )
                (msg 'error property "state definition - first expression of property must be user-defined symbol")
              )
              ;; check if second expression is a data type specifier
              (loop for typeS in typeSpecifiers while (not typeValid) do (setf typeValid (eq typeS type)))
              (unless typeValid
                (msg 'error property "state definition - second expression of property must be a type-specifier symbol ~s" 
                  `'(boolean bit string character integer float))
              )
              ;; when name and type are valid, create global variable for property and push it to stat?e definition
              (when (and nameValid typeValid)
                (setf stateDefinition (cons `(,name ,type) stateDefinition))
              )
            )
            (msg 'error property 
              "state definition - expected two symbol expressions (<property-name> <property-type>) in property but found ~d" (length property))
          )
          (msg 'error property "state definition - property must be a list in the form (<property-name> <property-type>)")
        )
      )
      (msg 'error read-initialState "state definition - state definition list must not be empty")
    )
    (msg 'error read-initialState "state definition - state definition must be a list in the form ((<property-name> <property-type>) ... )")
  )
)


(defun parseStates(read-initialState read-goalState)
  ;; don't parse initial and goal states until previous state definition is parsed successfully (for less convoluted error log)
  (unless errorsDetected
    (parseState read-initialState 'initialState "initial state")
    (parseState read-goalState 'goalState "goal state")
  )
)


(defun parseState(read-state state stateString)
  (msg 'debug nil "parsing ~a..." stateString)
  ;; check if state is formatted as list
  (if (typep read-state 'list)
    ;; check validity of each property
    (loop for property in read-state do
      ;; check if property is formatted as list
      (if (typep property 'list)
        ;; check if property list contains exactly two expressions
        (if (= 2 (length property))
          (let ((name (nth 0 property)) (value (getValue(nth 1 property)))  (type (nth 1 (assoc (nth 0 property) stateDefinition))))
            ;; check if first expression is a non-boolean symbol
            (if (and (typep name 'symbol) (not (typep name 'boolean)))
              ;; check if first expression is a symbol of a name of a property in the state definition
              (if (assoc name stateDefinition)
                ;; check if property has not already been assigned a value
                (if (not (eval `(assoc ',name ,state)))
                  ;; check if second expression is a value that matches the expected data type of the given property
                  (if (typep value type)
                    ;; save property name and value
                    (eval `(setf ,state (cons '(,name ,value) ,state)))
                    (msg 'error property "~a - expected ~a expression value for ~a property" stateString (string type) (string name))
                  )
                  (msg 'error read-state "~a - the value for property ~a must only be set once" stateString (string name))
                )
                (msg 'error property "~a - a property with the name ~a is not declared in the state definition" stateString (string name))
              )
              (msg 'error property "~a - property name must be symbol for the name of a property declared in the state definition" stateString)
            )
          )
          (msg 'error property "~a - expected two expressions (<property-name> <property-value>) in property but found ~d" stateString (length property))
        )
        (msg 'error property "~a - property must be a list in the form (<property-name> <property-value>)" stateString)
      )
    )
    (msg 'error read-state "~a must be a list in the form ((<property-name> <property-value>) ... )" stateString)
  )
  ;; check if state values have been set for all properties in the state definition
  (let ((missingProperties (getMissingProperties (eval state))))   
    (when missingProperties
      (msg 'error nil "~a - expected ~d properties declared in state definition - missing ~a" stateString (length stateDefinition) `',missingProperties)
    )
  )
)


(defun parseProductionRules(read-productionRules)
  ;; don't parse production rules until previous parameters parsed successfully (for less convoluted error log)
  (unless errorsDetected
    (msg 'debug nil "parsing production rules...")
    ;; check if production rules is a list
    (if (typep read-productionRules 'list)
      ;; check if production rules list is not empty
      (if (< 0 (length read-productionRules))
        ;; check validity of each production, ensure logic of expressions align with the properties of the state definition
        (loop for i below (length read-productionRules) do
          (parseProduction (nth i read-productionRules) (+ 1 i))
        )
        (msg 'error nil "production rules - production rules list must not be empty")
      )
      (msg 'error read-productionRules
        "production rules - production rules must be a list in the form ((<condition-semantics> <condition> <action-semantics> <action>) ... )")
    )
    (unless errorsDetected
      (setf productionRules read-productionRules)
    )
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
        (unless (typep (getValue condition-semantics) 'string)
          (msg 'error condition-semantics "production #~d - condition semantics of production must be a string expression" index)
        )
        ;; check action semantics
        (unless (typep (getValue action-semantics) 'string)
          (msg 'error action-semantics "production #~d - action semantics of production must be a string expression" index)
        )
        ;; test if condition and action are are valid using values of initial state to create dynamic local variables for state properties 
        (eval `(let ,initialState
          (declare (special ,@(mapcar #'first initialState)))

          ;; check condition
          (handler-case
            (unless (typep ,condition 'boolean)
              (msg 'error ',condition "production ~#d - expected a boolean expression for condition of production" ,index)
            )
            (error (e)
              (msg 'error ',condition 
                "production #~d - unable to evaluate condition of production - please verify the symbols and syntax " ,index)
            )
          )
            ;; check action
          (if (typep ',action 'list)
            (loop for property in ',action do
              ;; check if property is a list
              (if (typep property 'list)
                ;; check if property list has exactly two expression
                (if (= 2 (length property))
                  (let ((name (nth 0 property)) (value (getValue (nth 1 property)))  (type (nth 1 (assoc (nth 0 property) ',stateDefinition))))
                    ;; check if first expression is a non-boolean symbol
                    (if (and (typep name 'symbol) (not (typep name 'boolean)))
                      ;; check if first expression is a symbol of a name of a property is the state definition
                      (if (assoc name ',stateDefinition)
                        ;; check if second expression is a value that matches the expected data type of the given property
                        (unless (typep value type)
                          (msg 'error property 
                            "production #~d - expected ~a expression value for ~a action property" ,index (string type) (string name))
                        )
                        (msg 'error property "production #~d - a property with the name ~a is not declared in the state definition" 
                          ,index (string name))
                      )
                      (msg 'error property 
                        "production #~d - property name must be symbol for the name of a property declared in the state definition" ,index)
                    )
                  )
                  (msg 'error ',action 
                    "production #~d - expected two expressions (<property-name> <new-property-value>) in action property but found ~d"
                     ,index (length property))
                )
                (msg 'error ',action "production #~d - property update must be a list in the form (<property-name> <new-property-value>)" ,index)
              )
            )
            (msg 'error ',action "production #~d - action must be list in the form ((<property-name> <new-property-value>) ... )" ,index)
          )
        ))
      )
      (msg 'error nil "production #~d - expected 4 parameters in production but found ~d" index (length production))
    )
    (msg 'error property "production #~d - production must be a list in the form (<condition-semantics> <condition> <action-semantics> <action>)" index)
  )
)


(defun getValue(expression)
  ;; try to evaluate the expression and return its value
  (handler-case
    (return-from getValue (eval expression))
    (error (e1)
      (handler-case
        (return-from getValue expression)
        (error (e2)
          (msg 'error expression "I cannot even begin to imagine what you must have inputted to see this message. God save us all.")
        )
      )
    )
  )
)


;; retrieve list of of properties missing from inital and goals states
(defun getMissingProperties(target)
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


(defun reset()
  ;; reset paramaters
  (setf errorsDetected nil)
  (setf stateDefinition nil)
  (setf initialState nil)
  (setf goalState nil)
  (setf productionRules nil)
)


