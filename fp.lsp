
;; State definition
;; - define a list of pairs ((<property-name> <property-data-type>) ... ) to specify the properties required to represent a state of the problem instance
;; - you may define as many properties as you like, name them as you please, and use the data type from (boolean bit string character integer float)
;; - ex. ((a boolean) (b integer) (c string))
((farmer string) (wolf string) (goat string) (cabbage string))


;; Initial state
;; - define a list of pairs ((<property-name> <property-goal-value>) ... ) for each state definition property using its name and a value of its data type
;; - ex. ((a t) (b 1) (c "hello"))
((wolf "W") (farmer "W") (cabbage "W") (goat "W"))


;; Goal state
;; - define a list of pairs ((<property-name> <property-goal-value>) ... ) for each state definition property using its name and a value of its data type
;; - ex. ((a nil) (b 0) (c "goodbye"))
((cabbage "E") (goat "E") (wolf "E") (farmer "E"))


;; Production-rules
;; - define a list of production sublists where each sublist must define the four parameters in the following order:
;;    - condition-semantics: string expression that semantically describes the logic of the 'condition' in the context of the problem instance
;;    - condition: boolean expression that logically represents the condition to execute the 'action', and may use the state definition properties
;;    - action-semantics: string expression that semantically describes the 'action' in the context of the problem instance
;;    - action: list of pairs ((<property-name> (<new-property-value>)) ... ) to represent the updated state using the state definition properties
;;      - you need only to specify the properties that should be updated - those not specified will maintain their value in the updated state
;; - in addition to the state properties, the 'condition' and 'action' may use any Common Lisp functionality given they conform to their expected formats
;; - to simplify boolean expressions, (xor a b) and (xand a b ...) are both internally defined
;; - it is your responsibility to ensure your expressions are non-redundant and correct in the context of the problem, and safe to evaluate
(
 ("Farmer, goat, and cabbage are on the West side"
  (and (string= farmer "W") (string= goat "W") (string= cabbage "W"))
  "farmer travels with the goat to the East side"
  ((farmer "E") (goat "E")))

 ("Farmer, goat, and cabbage are on the West side and wolf is on the East side"
  (and (string= farmer "W") (string= goat "W") (string= cabbage "W") (string= wolf "E"))
  "farmer travels with the cabbage to the East side"
  ((farmer "E") (cabbage "E")))

 ("Farmer, goat, and cabbage are on the East side"
  (and (string= farmer "E") (string= goat "E") (string= cabbage "E"))
  "farmer travels with the goat to the West side"
  ((farmer "W") (goat "W")))

 ("Farmer, goat, and cabbage are on the East side and wolf is on the West side"
  (and (string= farmer "E") (string= goat "E") (string= cabbage "E") (string= wolf "W"))
  "farmer travels with the cabbage to the West side"
  ((farmer "W") (cabbage "W")))

 ("Farmer, wolf, and goat are on the West side and cabbage is on the East side"
  (and (string= farmer "W") (string= wolf "W") (string= goat "W") (string= cabbage "E"))
  "farmer travels with the wolf to the East side"
  ((farmer "E") (wolf "E")))

 ("Farmer, wolf, and goat are on the West side"
  (and (string= farmer "W") (string= wolf "W") (string= goat "W"))
  "farmer travels with the goat to the East side"
  ((farmer "E") (goat "E")))

 ("Farmer, wolf, and goat are on the East side and cabbage is on the West side"
  (and (string= farmer "E") (string= wolf "E") (string= goat "E") (string= cabbage "W"))
  "farmer travels with the wolf to the West side"
  ((farmer "W") (wolf "W")))

 ("Farmer, wolf, and goat are on the East side"
  (and (string= farmer "E") (string= wolf "E") (string= goat "E"))
  "farmer travels with the goat to the West side"
  ((farmer "W") (goat "W")))

 ("Farmer and goat are on the West side, wolf and cabbage are on the East side"
  (and (string= farmer "W") (string= goat "W") (string= wolf "E")  (string= cabbage "E"))
  "farmer travels alone to the East side"
  ((farmer "E")))

 ("Farmer and goat are on the East side, wolf and cabbage are on the West side"
  (and (string= farmer "E") (string= goat "E") (string= wolf "W")  (string= cabbage "W"))
  "farmer travels alone to the West side"
  ((farmer "W")))

 ("Farmer and goat are on the West side, wolf and cabbage are on the East side"
  (and (string= farmer "W") (string= goat "W") (string= wolf "E")  (string= cabbage "E"))
  "farmer travels alone to the East side"
  ((farmer "E")))
)
