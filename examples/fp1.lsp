;; The following file specifies the knowledge-base for a problem instance using a customizable state representation and set of production rules
;; Four parameters must be defined in the following order - state definition, initial state, goal state, production rules
;; All parameters must be constructed as raw lists (without the use of 'list' keyword, apostrophes, backticks, etc.) in the formats described below 


;; State definition
;; - list of pairs ((<property-name> <property-data-type>) ... ) to specify the properties required to represent a state of the problem instance
;; - you may define as many properties as you like and name them as you please
;; - data types may consist of boolean, bit, string, character, integer, float, and their aliases
( (farmer boolean) (wolf boolean) (goat boolean) (cabbage boolean) )


;; Initial state
;; - list of pairs ((<property-name> <property-goal-value>) ... ) for each state definition property using its name and an expression of its data type
( (wolf nil) (farmer nil) (cabbage nil) (goat (string= "you can use" "complex expressions!")) )


;; Goal state
;; - list of pairs ((<property-name> <property-goal-value>) ... ) for each state definition property using its name and an expression of its data type
( (cabbage t) (goat t) (wolf t) (farmer (or (= 1 0) (not (= 0 1)))) )


;; Production-rules
;; - list of production sublists where each sublist must define the four parameters in the following order:
;;    - condition-semantics: string expression that semantically describes the logic of the 'condition' in the context of the problem instance
;;    - condition: boolean expression that logically represents the condition to execute the 'action', and may use the state definition properties
;;    - action-semantics: string expression that semantically describes the 'action' in the context of the problem instance
;;    - action: list of pairs ((<property-name> (<new-property-value>)) ... ) to represent the updated state using the state definition properties
;;      - you need only to specify the properties that should be updated - those not specified will maintain their value in the updated state
;; - in addition to the state properties, the 'condition' and 'action' may use any Common Lisp functionality given they conform to their expected formats
;; - to simplify boolean expressions, (xor p q) and (xand p q ...) are both internally defined and may be used
;; - it is your responsibility to ensure your expressions are safe to evaluate, non-redundant and correct in the context of the problem instance
(
 ("Farmer, wolf, goat, and cabbage are on the same side"
  (xand farmer wolf goat cabbage)
  "farmer travels with the goat to the other side"
  ( (farmer (not farmer)) (goat (not goat)) ))

 ;;

 ("Farmer and goat are on one side, wolf and cabbage are on the other side"
  (and (xand farmer goat) (xand wolf cabbage) (xor farmer wolf))
  "farmer travels alone to the other side"
  ( (farmer (not farmer)) ))

 ("Farmer and goat are on one side, wolf and cabbage are on the other side"
  (and (xand farmer goat) (xand wolf cabbage) (xor farmer wolf))
  "farmer travels with the goat to the other side"
  ( (farmer (not farmer)) (goat (not goat)) ))

 ;;

 ("Farmer, wolf, and cabbage are one side, goat is on the other side"
  (and (xand farmer wolf cabbage) (xor farmer goat))
  "farmer travels with the wolf to the other side"
 ( (farmer (not farmer)) (wolf (not wolf)) ))

 ("Farmer, wolf, and cabbage are one side, goat is on the other side"
  (and (xand farmer wolf cabbage) (xor farmer goat))
  "farmer travels with the cabbage to the other side"
  ( (farmer (not farmer)) (cabbage (not cabbage)) ))

 ("Farmer, wolf, and cabbage are one side, goat is on the other side"
  (and (xand farmer wolf cabbage) (xor farmer goat))
  "farmer travels alone to the other side"
  ( (farmer (not farmer)) ))

 ;;

 ("Farmer, wolf, and goat are on one side, cabbage is on the other side"
  (and (xand farmer wolf goat) (xor farmer cabbage))
  "farmer travels with wolf to the other side"
  ( (farmer (not farmer)) (wolf (not wolf)) ))

 ("Farmer, wolf, and goat are on one side, cabbage is on the other side"
  (and (xand farmer wolf goat) (xor farmer cabbage))
  "farmer travels with goat to the other side"
  ( (farmer (not farmer)) (goat (not goat)) ))

 ;;

 ("Farmer, goat, and cabbage are on one side, wolf is on the other side"
  (and (xand farmer goat cabbage) (xor farmer wolf))
  "farmer travels with goat to the other side"
  ( (farmer (not farmer)) (goat (not goat)) ))

 ("Farmer, goat, and cabbage are on one side, wolf is on the other side"
  (and (xand farmer goat cabbage) (xor farmer wolf))
  "farmer travels with cabbage to the other side"
  ( (farmer (not farmer)) (cabbage (not cabbage)) ))
)
