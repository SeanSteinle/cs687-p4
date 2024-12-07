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
    (to-operator (car selected-subgoal))
    (precondition (cdr selected-subgoal)) 
    (from-operator (all-effects precondition myplan))) ;find an operator which will achieve our random subgoal
    (format t "random-subgoal chose a random goal from initial state: ~a~%" precondition)
    (format t "random-operator-with-subgoal chose an operator for subgoal (~a): ~a~%" precondition from-operator)
    (hook-up-operator from-operator to-operator precondition myplan 0 10 t) ;final 3 args: curr-depth, max-depth, new-op-was-added
    (format t "new plan after step with hook-up-operator: ~a~%" (print-plan myplan nil 0))
)


#|
TODO:
- test an isolated case of hooking operators to ensure that we can mutate the plan safely/correctly
- better understand the recursive nature/high-level structure of how function calls are made. should enable solving the 2-world blocksworld, but no optimizations
 |#