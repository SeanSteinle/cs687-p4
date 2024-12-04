(declaim (sb-ext:muffle-conditions cl:warning))
(load "p4.lisp")

;UTILITY FUNCTIONS

;;negate
(let ((pred '(t a-on-table)))
    (assert (first pred))
    (assert (not (first (negate pred))))
)

;;reachable
(let ((assoc-list '((a . b) (b . c) (c . d) (d . e))))
  (assert (reachable assoc-list 'a 'e))
  (assert (not (reachable assoc-list 'e 'a))))

;;cyclic-assoc-list
(let ((assoc-list1 '((a . b) (b . c) (c . d) (d . a))) (assoc-list2 '((a . b) (b . c) (c . d) (d . e))))
  (assert (cyclic-assoc-list assoc-list1))
  (assert (not (cyclic-assoc-list assoc-list2))))

;;binary-combinations
(assert (= (length (binary-combinations 3)) (expt 2 3)))
;(format t "Binary Combinations for n=3: ~a" (binary-combinations 3))

;HIGH LEVEL GOALS

;;do-pop 
;NOTE: you can do tests and such on initial plan, which is provided!
;for now, just keep working through the algorithm top-down
;currently working on select subgoal!
;(do-pop)

;get specific struct members like: (plan-operators myplan). that's equivalent to myplan.operators

;SELECT SUBGOAL FUNCTIONS

;;random-precondition (returns operator (s_need) and its random precondition (c) we selected)
(let* (
    (myplan (make-initial-plan))
    (selected-subgoal (random-subgoal myplan))
    (need-operator (first selected-subgoal))
    (precondition (second selected-subgoal))
    (add-operator (random-operator-with-subgoal myplan precondition)))
    ;selected subgoal ;subgoal
    (format t "chose random subgoal: ~a~%" precondition)
    (if (member precondition *goal-preconditions* :test #'equal) t nil)

    (format t "chose random fulfilling action: ~a~%" (random-operator-with-subgoal myplan precondition))
)

#| 
now that we have a random subgoal, we need to find all of the possible operators which could accomplish that subgoal.
to do this, we really just need to ask:
for operator in operators:
    if subgoal in operators.effects:
        valid_operators.append(operator)
return random_element(valid_operators)

(first (plan-operators myplan))
(operator-effects (first (plan-operators myplan))) ;accesses each operator's effects where the operator is the (first ...) clause
|#

;;operators-with-effect
(eq (operator-name (first (operators-with-effect (plan-operators (make-initial-plan)) '(t a-on-table)))) 'start)
;(random-operator-with-subgoal (make-initial-plan) '(t a-on-table)) CAN CALL LIKE THIS



;(operators-with-effect (plan-operators myplan) (second (random-subgoal myplan)))

;(plan-operators myplan) ;operators
;(second (random-subgoal myplan)) ;subgoal