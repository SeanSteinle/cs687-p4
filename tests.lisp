(declaim (sb-ext:muffle-conditions cl:warning))
(load "p4.lisp")

#|
Data Structures Notes:
- you can get specific struct members like: (plan-operators myplan). that's equivalent to myplan.operators
 |#

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

;SELECT SUBGOAL FUNCTIONS

;;random-precondition and random-operator-with-subgoal
(let* (
    (myplan (make-initial-plan))
    (selected-subgoal (pick-precond myplan)) ;choose a random subgoal, get its operator and precondition
    (ss-operator (car selected-subgoal))
    (ss-precondition (cdr selected-subgoal)) 
    (co-operator (all-effects ss-precondition myplan))) ;find an operator which will achieve our random subgoal
    (format t "random-subgoal chose a random goal from initial state: ~a~%" ss-precondition)
    (format t "random-operator-with-subgoal chose an operator for subgoal (~a): ~a~%" ss-precondition co-operator)
)

#|
TODO:
- return as cons not list out of pick-precond

NOTE: compare how Professor Luke's Lisp template contrasts the POP.pdf file online.
 |#