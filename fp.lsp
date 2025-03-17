
;; State definition
;; - define a list of pairs ((<property-name> <property-data-type>) ... ) to specify the properties required to represent a state of the problem instance
;; - you may define as many properties as you like, name them as you please, and use the data type from (boolean bit string character integer float)
;; - ex. ((a boolean) (b integer) (c string))
((farmer boolean) (wolf boolean) (goat boolean) (cabbage boolean))


;; Initial state
;; - define a list of pairs ((<property-name> <property-initial-value>) ... ) for each property in the state definition using its appropriate data type
;; - ex. ((a t) (b 1) (c "hello"))
((wolf nil) (farmer nil) (cabbage nil) (goat nil)  )


;; Goal state
;; - define a list of pairs ((<property-name> <property-goal-value>) ... ) for each property in the state definition using its appropriate data type
;; - ex. ((a nil) (b 0) (c "goodbye"))
((cabbage t) (goat t) (wolf t) (farmer t))


;; Production-rules
;; - define a list of production sublists where each sublist must define the four parameters in the following order:
;;    - condition-semantics: string expression that semantically describes the logic of the 'condition' in the context of the problem instance
;;    - condition: boolean expression that logically represents the constraints of the 'action', and may use the state definition properties
;;    - action-semantics: string expression that semantically describes the 'action' in the context of the problem instance
;;    - action: list of pairs ((<property-name> (<new-property-value>)) ... ) to represent the updated state using the state definition properties
;;      - you need only to specify the properties that should be updated - those not specified will maintain their value in the updated state
;; - in addition to the state properties, the 'condition' and 'action' may use any Common Lisp functionality given they conform to their expected formats
;; - it is your responsibility to ensure all expressions are both syntactically correct and safe to evaluate
(
  (
    "wolf and goat are not together and goat and cabbage are not together"
    (and (not (xor wolf goat))  (not (xor goat cabbage)))
    "farmer crosses to other side of river alone"
    ((farmer (not farmer)))
  )
)
