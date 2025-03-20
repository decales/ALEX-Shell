;; The following file specifies the knowledge-base for a problem instance using a customizable state representation and set of production rules
;; Four parameters must be defined in the following order - state definition, initial state, goal state, production rules
;; All parameters must be constructed as raw lists (without the use of 'list' keyword, apostrophes, backticks, etc.) in the formats described below 


;; State definition
;; - define a list of pairs ((<property-name> <property-data-type>) ... ) to specify the properties required to represent a state of the problem instance
;; - you may define as many properties as you like and name them as you please
;; - data types may consist of boolean, bit, string, character, integer, float, and their aliases
( (farmer character) (wolf character) (goat character) (cabbage character) )


;; Initial state
;; - define a list of pairs ((<property-name> <property-goal-value>) ... ) for each state definition property using its name and a value of its data type
( (wolf #\W) (farmer #\W) (cabbage #\W) (goat #\W) )


;; Goal state
;; - define a list of pairs ((<property-name> <property-goal-value>) ... ) for each state definition property using its name and a value of its data type
( (cabbage #\E) (goat #\E) (wolf #\E) (farmer #\E) )


;; Production-rules
;; - define a list of production sublists where each sublist must define the four parameters in the following order:
;;    - condition-semantics: string expression that semantically describes the logic of the 'condition' in the context of the problem instance
;;    - condition: boolean expression that logically represents the condition to execute the 'action', and may use the state definition properties
;;    - action-semantics: string expression that semantically describes the 'action' in the context of the problem instance
;;    - action: list of pairs ((<property-name> (<new-property-value>)) ... ) to represent the updated state using the state definition properties
;;      - you need only to specify the properties that should be updated - those not specified will maintain their value in the updated state
;; - in addition to the state properties, the 'condition' and 'action' may use any Common Lisp functionality given they conform to their expected formats
;; - to simplify boolean expressions, (xor p q) and (xand p q ...) are both internally defined and may be used 
;; - it is your responsibility to ensure your expressions are safe to evaluate, non-redundant, and correct in the context of the problem instance
(
 ("Farmer, wolf, goat, and cabbage are on the West side"
  (and (eq #\W farmer) (char= #\W wolf) (char= #\W goat) (char= #\W farmer))
  "farmer travels with the goat to the East side"
  ( (farmer #\E) (goat #\E) ))

 ;;
 
 ("Farmer and goat are on the West side, wolf and cabbage are on the East side"
  (and (char= #\W farmer) (char= #\E wolf) (char= #\W goat) (char= #\E cabbage))
  "farmer travels alone to the East side"
  ( (farmer #\E) ))

 ("Farmer and goat are on the East side, wolf and cabbage are on the West side"
  (and (char= #\E farmer) (char= #\W wolf) (char= #\E goat) (char= #\W cabbage))
  "farmer travels alone to the West side"
  ( (farmer #\W) ))

 ("Farmer and goat are on the West side, wolf and cabbage are on the East side"
  (and (char= #\W farmer) (char= #\E wolf) (char= #\W goat) (char= #\E cabbage))
  "farmer travels with the goat to the East side"
  ( (farmer #\E) (goat #\E) ))

 ("Farmer and goat are on the East side, wolf and cabbage are on the West side"
  (and (char= #\E farmer) (char= #\W wolf) (char= #\E goat) (char= #\W cabbage))
  "farmer travels with the goat to the West side"
  ( (farmer #\W) (goat #\W) ))

 ;;

 ("Farmer, wolf, and cabbage are on the West side, goat is on the East side"
  (and (char= #\W farmer) (char= #\W wolf) (char= #\E goat) (char= #\W cabbage))
  "farmer travels with the wolf to the East side"
  ( (farmer #\E) (wolf #\E) ))

 ("Farmer, wolf, and cabbage are on the East side, goat is on the West side"
  (and (char= #\E farmer) (char= #\E wolf) (char= #\W goat) (char= #\E cabbage))
  "farmer travels with the wolf to the West side"
  ( (farmer #\W) (wolf #\W) ))

 ("Farmer, wolf, and cabbage are on the West side, goat is on the East side"
  (and (char= #\W farmer) (char= #\W wolf) (char= #\E goat) (char= #\W cabbage))
  "farmer travels with the cabbage to the East side"
  ( (farmer #\E) (cabbage #\E) ))

 ("Farmer, wolf, and cabbage are on the East side, goat is on the West side"
  (and (char= #\E farmer) (char= #\E wolf) (char= #\W goat) (char= #\E cabbage))
  "farmer travels with the cabbage to the West side"
  ( (farmer #\W) (cabbage #\W) ))

 ("Farmer, wolf, and cabbage are on the West side, goat is on the East side"
  (and (char= #\W farmer) (char= #\W wolf) (char= #\E goat) (char= #\W cabbage))
  "farmer travels alone to the East side"
  ( (farmer #\E) ))

 ("Farmer, wolf, and cabbage are on the East side, goat is on the West side"
  (and (char= #\E farmer) (char= #\E wolf) (char= #\W goat) (char= #\E cabbage))
  "farmer travels alone to the West side"
  ( (farmer #\W) ))

 ;;

 ("Farmer, wolf, and goat are on the West side, cabbage is on the East side"
  (and (char= #\W farmer) (char= #\W wolf) (char= #\W goat) (char= #\E cabbage))
  "farmer travels with the wolf to the East side"
 ( (farmer #\E) (wolf #\E) ))

 ("Farmer, wolf, and goat are on the East side, cabbage is on the West side"
  (and (char= #\E farmer) (char= #\E wolf) (char= #\E goat) (char= #\W cabbage))
  "farmer travels with the wolf to the West side"
 ( (farmer #\W) (wolf #\W) ))

 ("Farmer, wolf, and goat are on the West side, cabbage is on the East side"
  (and (char= #\W farmer) (char= #\W wolf) (char= #\W goat) (char= #\E cabbage))
  "farmer travels with the goat to the East side"
 ( (farmer #\E) (goat #\E) ))

 ("Farmer, wolf, and goat are on the East side, cabbage is on the West side"
  (and (char= #\E farmer) (char= #\E wolf) (char= #\E goat) (char= #\W cabbage))
  "farmer travels with the goat to the West side"
 ( (farmer #\W) (goat #\W) ))

 ;;

 ("Farmer, goat, and cabbage are on one the West side, wolf is on the East side"
  (and (char= #\W farmer) (char= #\E wolf) (char= #\W goat) (char= #\W cabbage))
  "farmer travels with the goat to the East side"
 ( (farmer #\E) (goat #\E) ))

 ("Farmer, goat, and cabbage are on one the East side, wolf is on the West side"
  (and (char= #\E farmer) (char= #\W wolf) (char= #\E goat) (char= #\E cabbage))
  "farmer travels with the goat to the West side"
 ( (farmer #\W) (goat #\W) ))

 ("Farmer, goat, and cabbage are on one the West side, wolf is on the East side"
  (and (char= #\W farmer) (char= #\E wolf) (char= #\W goat) (char= #\W cabbage))
  "farmer travels with the cabbage to the East side"
 ( (farmer #\E) (cabbage #\E) ))

 ("Farmer, goat, and cabbage are on one the East side, wolf is on the West side"
  (and (char= #\E farmer) (char= #\W wolf) (char= #\E goat) (char= #\E cabbage))
  "farmer travels with the cabbage to the West side"
 ( (farmer #\W) (cabbage #\W) ))
)
