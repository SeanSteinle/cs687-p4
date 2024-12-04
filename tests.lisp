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

(let ((myplan (make-initial-plan)))
    (random-precondition myplan)
)
(format t "initial plan:~% ~a" (print-plan myplan nil 0))
;get specific struct members like: (plan-operators myplan). that's equivalent to myplan.operators

;SELECT SUBGOAL FUNCTIONS

;;random-precondition (returns operator (s_need) and its random precondition (c) we selected)
(let* (
    (selected-precondition (random-precondition myplan))
    (operator (first selected-precondition))
    (precondition (second selected-precondition)))  ;should return T
  (if (member precondition *goal-preconditions* :test #'equal) t nil))
