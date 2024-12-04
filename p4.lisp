;;;;; PREDICATES

;;; A predicate is a list of the form (t-or-nil name)
;;; where the t-or-nil indicates if it's negated,
;;; and NAME is a symbol indicating the predicate name.
;;; Example: (NIL B-ON-A)  "B is not on A"

(defun negate (predicate)
  "Negates a predicate.  Pretty simple!"
  (cons (not (first predicate)) (rest predicate)))


;;;;;; STRIPS OPERATORS
;;;;;; A strips operator has is described below.  Note that
;;;;;; UNIQ allows two operators with the same name, preconds
;;;;;; and effects to be in the plan at once.

(defstruct operator
  "Defines a strips operator consisting of
a NAME (a symbol or string),
a UNIQ gensym symbol assigned when the operator is added to the plan,
a list of PRECONDITIONS (predicates)
a list of EFFECTS ( predicates),

The resultant function MAKE-OPERATOR creates a *template*,
which is different from an instantiated operator actually in a plan.
Use instantiate-operator to create an operator from a template."
  name uniq preconditions effects)


;; Expect a possible warning here about redefinition
(defun copy-operator (operator)
  "Copies the operator and assigns it a new unique gensym symbol to make
it unique as far as EQUALP is concerned.  Returns the copy."
  ;; I suggest you use this code to guarantee that the operator is properly
  ;; copied out of the templates.
  (let ((op (copy-structure operator)))
    (setf (operator-uniq op) (gensym))
    op))



;;;;;;; LINKS
;;;;;;; A link is a structure that specifies a causal link with a from-operator,
;;;;;;; a to-operator, and precondition of the to-operator involved
;;;;;;; (which is the SAME as the effect of the from-operator!)

(defstruct (link
             (:print-function print-link))
  "FROM and TO are operators in the plan.
  PRECOND is the predicate that FROM's effect makes true in TO's precondition."
  from precond to)

(defun print-link (p stream depth)
  "Helper function to print link in a pretty way"
  (declare (ignorable depth))
  (format stream "#< (~a)~a -> (~a)~a : ~a >"
          (when (link-from p) (operator-uniq (link-from p)))
          (when (link-from p) (operator-name (link-from p)))
          (when (link-to p) (operator-uniq (link-to p)))
          (when (link-to p) (operator-name (link-to p)))
          (link-precond p)))


;;;;;;; ORDERINGS
;;;;;;; An ordering is just a dotted pair of the form (before-op . after-op)
;;;;;;; where before-op and after-op are strips operators (instances of
;;;;;;; the OPERATOR structure).  The ordering specifies
;;;;;;; that before-op must come before after-op.


(defun print-ordering (p stream depth)
  "Helper function to print link in a pretty way"
  (declare (ignorable depth))
  (format stream "#[ (~a)~a -> (~a)~a ]"
          (operator-uniq (first p))
          (operator-name (first p))
          (operator-uniq (rest p))
          (operator-name (rest p))))


;;;;;;; PLANS
;;;;;;; A plan is a list of operators, a list of orderings, and a list of
;;;;;;; links, plus the goal and start operator (which are also in the operator
;;;;;;; list).

(defstruct (plan (:print-function print-plan))
  "A collection of lists of operators, orderings, and links,
plus a pointer to the start operator and to the goal operator."
  operators orderings links start goal)

(defun print-plan (p stream depth)
  "Helper function print plan in a pretty way"
  (declare (ignorable depth))
  (format stream "#< PLAN operators: ~{~%~a~} ~%links: ~{~%~a~} ~%orderings: ~{~%~a~}~%>"
          (plan-operators p) (plan-links p)
          (mapcar #'(lambda (ordering)
                      (print-ordering ordering nil 0))
                  (plan-orderings p))))

;; Expect a possible warning here about redefinition
(defun copy-plan (plan)
  ;; I suggest you use this code to guarantee that the plan is copied
  ;; before you do any destructive coding on it.
  "Deep-copies the plan, and copies the operators, orderings, and links."
  (let ((p (copy-structure plan)))
    (setf (plan-operators p) (copy-tree (plan-operators p)))
    (setf (plan-orderings p) (copy-tree (plan-orderings p)))
    (setf (plan-links p) (copy-tree (plan-links p)))
    p))




;;;;;;;;; UTILITY FUNCTIONS
;;;;;;;;; I predict you will find these functions useful.

;;;; Reachable takes an association list and determines if you can reach
;;;; an item in the list from another item.  For example:
;;;;
;;;; (reachable '((a . b) (c . d) (b . c) (b . e) (e . a)) 'e 'd)
;;;; --> T   ;; e->a, a->b, b->c, c->d

(defun reachable (assoc-list from to)
  "Returns t if to is reachable from from in the association list."
  ;; expensive!

;;; SPEED HINT.  You might rewrite this function to be more efficient.
;;; You could try dividing the list into two lists, one consisting of association pairs
;;; known to be reachable from FROM and ones not know to be reachable, then
;;; using the property of transitivity, move pairs from the second list to the first
;;; list, until either you discover it's reachable, or nothing else is moving.

  (dolist (x assoc-list nil)
    (when (and (equalp (car x) from)
               (or (equalp (cdr x) to)
                   (reachable (remove x assoc-list) (cdr x) to)))
      (return t))))


;;;; Cyclic-assoc-list takes an association list and determines if it
;;;; contains a cycle (two objects can reach each other)
;;;;
;;;; (cyclic-assoc-list '((a . b) (c . d) (b . c) (b . e) (e . a)))
;;;; --> T   ;; a->b, b->e, e->a
;;;;
;;;; (cyclic-assoc-list '((a . a)))
;;;; --> T   ;; a->a

(defun cyclic-assoc-list (assoc-list)
  (dolist (x assoc-list nil)
    (when (reachable assoc-list (cdr x) (car x))
      (return t))))

;;;; Binary-combinations returns all N^2 combinations of T and NIL.
;;;; 
;;;; (binary-combinations 4)
;;;; -->
;;;; ((NIL T NIL T) (T T NIL T)
;;;;  (NIL NIL NIL T) (T NIL NIL T)
;;;;  (NIL T T T) (T T T T)
;;;;  (NIL NIL T T) (T NIL T T)
;;;;  (NIL T NIL NIL) (T T NIL NIL)
;;;;  (NIL NIL NIL NIL) (T NIL NIL NIL)
;;;;  (NIL T T NIL) (T T T NIL)
;;;;  (NIL NIL T NIL) (T NIL T NIL))

(defun binary-combinations (n)
  "Gives all combinations of n t's and nils"
  (let ((bag '(())))
    (dotimes (x n bag)
      (let (bag2)
        (dolist (b bag)
          (push (cons t b) bag2)
          (push (cons nil b) bag2))
        (setf bag bag2)))))



;;;;;; PLANNING CODE TEMPLATES
;;;;;;
;;;;;; The following are the functions I used to implement my planner.
;;;;;; You have been given all interfaces I used, but no implementation.
;;;;;; This is how I went about doing it -- if you want to you can of course
;;;;;; do the code differently, but indicate that you are doing so.
;;;;;;
;;;;;; The recursion looks like this:
;;;;;; SELECT-SUBGOAL calls CHOOSE-OPERATOR, which calls HOOK-UP-OPERATOR,
;;;;;; which calls RESOLVE-THREATS, which in turn calls SELECT-SUBGOAL.
;;;;;; All of these functions return a either plan
;;;;;; (the solution) or NIL (failure to find a solution).
;;;;;; 
;;;;;; In SELECT-SUBGOAL I test to see if the maximum depth is exceeded
;;;;;; and fail immediately if it is.  That's the only place I check for
;;;;;; depth.  You are free to test other places if you want.  Then I
;;;;;; I increment the current depth before passing it to CHOOSE-OPERATOR.
;;;;;;
;;;;;; Some hints: I also used PUSH and SETF and DOLIST a lot,
;;;;;; as well as a healthy dose of MAPCAR, MAPC, and MAPL.  Also you might
;;;;;; want to read up on CONSes of the form (a . b) which I use a lot,
;;;;;; and also ASSOCIATION LISTS which are very convenient in certain spots


(defun before-p (operator1 operator2 plan)
  "Operator1 is ordered before operator2 in plan?"
;;; perhaps you have an existing function which could help here.
  )


(defun link-exists-for-precondition-p (precond operator plan)
  "T if there's a link for the precond for a given operator, else nil.
precond is a predicate."
  )


(defun operator-threatens-link-p (operator link plan)
  "T if operator threatens link in plan, because it's not ordered after
or before the link, and it's got an effect which counters the link's effect."
;;; SPEED HINT.  Test the easy tests before the more costly ones.
  )

(defun inconsistent-p (plan)
  "Plan orderings are inconsistent"
  ;; hint: cyclic-assoc-list
  )

(defun pick-precond (plan)
  "Return ONE (operator . precondition) pair in the plan that has not been met yet.
If there is no such pair, return nil"
;;; SPEED HINT.  Any precondition will work.  But this is an opportunity
;;; to pick a smart one.  Perhaps you might select the precondition
;;; which has the fewest possible operators which solve it, so it fails
;;; the fastest if it's wrong. 
  )

(defun all-effects (precondition plan)
  "Given a precondition, returns a list of ALL operators presently IN THE PLAN which have
effects which can achieve this precondition."
  ;; hint: there's short, efficient way to do this, and a long,
  ;; grotesquely inefficient way.  Don't do the inefficient way.
  )

(defun all-operators (precondition)
  "Given a precondition, returns all list of ALL operator templates which have
an effect that can achieve this precondition."
  ;; hint: there's short, efficient way to do this, and a long,
  ;; grotesquely inefficient way.  Don't do the inefficient way.
  )

(defun select-subgoal (plan current-depth max-depth)
  "For all possible subgoals, recursively calls choose-operator
on those subgoals.  Returns a solved plan, else nil if not solved."
          ;;; an enterprising student noted that the book says you DON'T have
          ;;; to nondeterministically choose from among all preconditions --
          ;;; you just pick one arbitrarily and that's all.  Note that the
          ;;; algorithm says "pick a plan step...", rather than "CHOOSE a
          ;;; plan step....".  This makes the algorithm much faster.  
  )

;helpers for select-subgoal
(defun random-elt (sequence)
  "Returns a random element from sequence"
  (elt sequence (random (length sequence))))

(defun operators-with-preconditions (operators)
  (remove-if-not #'(lambda (op)
                     (not (null (operator-preconditions op))))
                 operators))

(defun random-precondition (plan) ;NOTE: should be replaced with a heuristic based system at some point
  (let ((operator (random-elt (operators-with-preconditions (plan-operators plan)))))
     (random-elt (operator-preconditions operator))))

(defun choose-operator (op-precond-pair plan current-depth max-depth)
  "For a given (operator . precondition) pair, recursively call
hook-up-operator for all possible operators in the plan.  If that
doesn't work, recursively call add operators and call hook-up-operators
on them.  Returns a solved plan, else nil if not solved."
  )

(defun add-operator (operator plan)
  "Given an OPERATOR and a PLAN makes a copy of the plan [the
operator should have already been copied out of its template at this point].
Then adds that copied operator
the copied plan, and hooks up the orderings so that the new operator is
after start and before goal.  Returns the modified copy of the plan."
  ;;; hint: make sure you copy the plan!
  ;;; also hint: use PUSHNEW to add stuff but not duplicates
  ;;; Don't use PUSHNEW everywhere instead of PUSH, just where it
  ;;; makes specific sense.
  )

(defun hook-up-operator (from to precondition plan
                         current-depth max-depth
                         new-operator-was-added)
  "Hooks up an operator called FROM, adding the links and orderings to the operator
TO for the given PRECONDITION that FROM achieves for TO.  Then
recursively  calls resolve-threats to fix any problems.  Presumes that
PLAN is a copy that can be modified at will by HOOK-UP-OPERATOR. Returns a solved
plan, else nil if not solved."
  ;;; hint: want to go fast?  The first thing you should do is
  ;;; test to see if TO is already ordered before FROM and thus
  ;;; hooking them up would make the plan inconsistent from the get-go
  ;;; also hint: use PUSHNEW to add stuff but not duplicates  
  ;;; Don't use PUSHNEW everywhere instead of PUSH, just where it
  ;;; makes specific sense.
  )

(defun threats (plan maybe-threatening-operator maybe-threatened-link)
  "After hooking up an operator, we have two places that we need to check for threats.
First, we need to see if the link we just created is threatened by some operator.
Second, IF we just added in an operator, then we need to check to see if it threatens
any links.

This function should return a list of (op . link) pairs (called ''threats'') which
indicate all the situations where some operator OP threatens a link LINK.  The only
situations you need to check are the ones described in the previous paragraph.

This function should assume that if MAYBE-THREATENING-OPERATOR is NIL, then no
operator was added and we don't have to check for its threats.  However, we must
always check for any operators which threaten MAYBE-THREATENED-LINK."
  )


(defun all-promotion-demotion-plans (plan threats)
  "Returns plans for each combination of promotions and demotions
of the given threats, except  for the inconsistent plans.  These plans
are copies of the original plan."
  ;;; Hint: binary-combinations could be useful to you.
  ;;; Also check out MAPC
  ;;; SPEED HINT.  You might handle the one-threat case specially.
  ;;; In that case you could also check for inconsistency right then and there too.
  )

(defun promote (operator link plan)
  "Promotes an operator relative to a link.  Doesn't copy the plan."
  )

(defun demote (operator link plan)
  "Demotes an operator relative to a link.  Doesn't copy the plan."
  )

(defun resolve-threats (plan threats current-depth max-depth)
  "Tries all combinations of solutions to all the threats in the plan,
then recursively calls SELECT-SUBGOAL on them until one returns a
solved plan.  Returns the solved plan, else nil if no solved plan."
  )




;;;;;;; DO-POP
;;;;;;; This is the high-level code.  Note it creates a goal and a start
;;;;;;; operator, then creates a plan with those operators and an ordering
;;;;;;; between them.  Then it does iterative-deepening, calling
;;;;;;; SELECT-SUBGOAL with ever-larger maximum depths.  If the solution is
;;;;;;; non-null, it breaks out of the loop and returns the solution.
;;;;;;;
;;;;;;; One thing you should note is that DO-POP also builds a little hash
;;;;;;; table called *operators-for-precond*.  I think you will find this
;;;;;;; useful in one of your functions.

(defparameter *depth-increment* 1
  "The depth to increment in iterative deepening search")

;;; This is used to cache the operators by precond.  You might find this
;;; useful to make your ALL-OPERATORS code much much much faster than the
;;; obvious dorky way to do it.
(defparameter *operators-for-precond* nil
  "Hash table.  Will yield a list of operators which can achieve a given precondition")

(defun build-operators-for-precond ()
  "Builds the hash table"
  (setf *operators-for-precond* (make-hash-table :test #'equalp))
  (dolist (operator *operators*)
    (dolist (effect (operator-effects operator))
      (push operator (gethash effect *operators-for-precond*)))))

(defun make-initial-plan ()
  (let* ((start (make-operator
                 :name 'start
                 :uniq (gensym)
                 :preconditions nil
                 :effects *start-effects*))
         (goal (make-operator
                :name 'goal
                :uniq (gensym)
                :preconditions *goal-preconditions*
                :effects nil))
         (plan (make-plan
                :operators (list start goal)
                :orderings (list (cons start goal))
                :links nil
                :start start
                :goal goal)))
    return plan))
                

(defun do-pop ()
  (let* ((start (make-operator
                 :name 'start
                 :uniq (gensym)
                 :preconditions nil
                 :effects *start-effects*))
         (goal (make-operator
                :name 'goal
                :uniq (gensym)
                :preconditions *goal-preconditions*
                :effects nil))
         (plan (make-plan
                :operators (list start goal)
                :orderings (list (cons start goal))
                :links nil
                :start start
                :goal goal))
         (depth *depth-increment*)
         solution)
    (build-operators-for-precond)
    ;(format t "initial plan:~% ~a" (print-plan plan nil 0))
    ;; Do iterative deepening search on this sucker
    (loop
       (format t "~%Search Depth: ~d" depth)
       (setf solution (select-subgoal plan 0 depth)) ;should return step + some condition to focus on
       (format t "~%after selecting subgoal, got solution:~% ~a" solution)
       ;() ;choose-operator
       ;() ;resolve-threats
       (when solution (return)) ;; SHOULD REALLY be when goal open preconds is empty.
       (incf depth *depth-increment*))
    ;; found the answer if we got here
    (format t "~%Solution Discovered:~%~%") ;NOTE right now the loop only executes once because select-subgoal returns a str (T)
    solution))





;;;;; TWO-BLOCK-WORLD
;;;;; You have two blocks on the table, A and B.   Pretty simple, no?
(defparameter *operators*
  (list
   ;; move from table operators
   (make-operator :name 'a-table-to-b
                  :preconditions '((t a-on-table) (t b-clear) (t a-clear))
                  :effects '((nil a-on-table) (nil b-clear) (t a-on-b)))
   (make-operator :name 'b-table-to-a
                  :preconditions '((t b-on-table) (t a-clear) (t b-clear))
                  :effects '((nil b-on-table) (nil a-clear) (t b-on-a)))
   ;; move to table operators
   (make-operator :name 'a-b-to-table
                  :preconditions '((t a-on-b) (t a-clear))
                  :effects '((t a-on-table) (nil a-on-b) (t b-clear)))
   (make-operator :name 'b-a-to-table
                  :preconditions '((t b-on-a) (t b-clear))
                  :effects '((t b-on-table) (nil b-on-a) (t a-clear))))
  "A list of strips operators without their uniq gensyms set yet -- 
doesn't matter really -- but NOT including a goal or start operator")


;;; b is on top of a
(defparameter *start-effects*
  '((t a-on-table) (t b-on-a) (t b-clear)))

;;; a is on top of b
(defparameter *goal-preconditions*
  ;; somewhat redundant, is doable with just ((t a-on-b))
  '((t a-on-b) (t b-on-table) (t a-clear)))




;;;;; Solution to 2-BLOCK-WORLD on SBCL: 

;; * (do-pop)
;;
;; Search Depth: 1
;; Search Depth: 2
;; Search Depth: 3
;; Search Depth: 4
;; Search Depth: 5
;; Search Depth: 6
;; Search Depth: 7
;; Search Depth: 8
;; Search Depth: 9
;; Solution Discovered:
;;
;; #< PLAN operators: 
;; #S(OPERATOR
;;    :NAME A-TABLE-TO-B
;;    :UNIQ G251529
;;    :PRECONDITIONS ((T A-ON-TABLE) (T B-CLEAR) (T A-CLEAR))
;;    :EFFECTS ((NIL A-ON-TABLE) (NIL B-CLEAR) (T A-ON-B)))
;; #S(OPERATOR
;;    :NAME B-A-TO-TABLE
;;    :UNIQ G251527
;;    :PRECONDITIONS ((T B-ON-A) (T B-CLEAR))
;;    :EFFECTS ((T B-ON-TABLE) (NIL B-ON-A) (T A-CLEAR)))
;; #S(OPERATOR
;;    :NAME START
;;    :UNIQ G251258
;;    :PRECONDITIONS NIL
;;    :EFFECTS ((T A-ON-TABLE) (T B-ON-A) (T B-CLEAR)))
;; #S(OPERATOR
;;    :NAME GOAL
;;    :UNIQ G251259
;;    :PRECONDITIONS ((T A-ON-B) (T B-ON-TABLE) (T A-CLEAR))
;;    :EFFECTS NIL) 
;; links: 
;; #< (G251258)START -> (G251529)A-TABLE-TO-B : (T A-ON-TABLE) >
;; #< (G251258)START -> (G251529)A-TABLE-TO-B : (T B-CLEAR) >
;; #< (G251527)B-A-TO-TABLE -> (G251529)A-TABLE-TO-B : (T A-CLEAR) >
;; #< (G251258)START -> (G251527)B-A-TO-TABLE : (T B-ON-A) >
;; #< (G251258)START -> (G251527)B-A-TO-TABLE : (T B-CLEAR) >
;; #< (G251529)A-TABLE-TO-B -> (G251259)GOAL : (T A-ON-B) >
;; #< (G251527)B-A-TO-TABLE -> (G251259)GOAL : (T B-ON-TABLE) >
;; #< (G251527)B-A-TO-TABLE -> (G251259)GOAL : (T A-CLEAR) > 
;; orderings: 
;; #[ (G251527)B-A-TO-TABLE -> (G251529)A-TABLE-TO-B ]
;; #[ (G251529)A-TABLE-TO-B -> (G251259)GOAL ]
;; #[ (G251258)START -> (G251529)A-TABLE-TO-B ]
;; #[ (G251527)B-A-TO-TABLE -> (G251259)GOAL ]
;; #[ (G251258)START -> (G251527)B-A-TO-TABLE ]
;; #[ (G251258)START -> (G251259)GOAL ]
;; >








;;;;;; THREE-BLOCK-WORLD
;;;;;; You have three blocks on the table, A, B, and C.
;;;;;;
;;;;;;
;;;
;;; Why so many operators?  Because we don't have a variable facility.
;;; We can't say MOVE(x,y,z) -- we can only say MOVE(A,TABLE,B).  To
;;; add in a variable facility is a lot more coding, and I figured I'd
;;; save you the hassle of unification.  If you want to give it a shot,
;;; I have written up some unification code which might help you out.
;;; Another consequence of not having a variable facility is that you
;;; can't rely on the least-commitment heuristic of not immediately
;;; binding variables to constants.  For us, we must *immediately*
;;; commit to constants.  That makes our search space much nastier.
;;; C'est la vie!
;;;
;; (defparameter *operators*
;;   (list
;;    ;; move from table operators
;;    (make-operator :name 'a-table-to-b
;;                :preconditions '((t a-on-table) (t b-clear) (t a-clear))
;;                :effects '((nil a-on-table) (nil b-clear) (t a-on-b)))
;;    (make-operator :name 'a-table-to-c
;;                :preconditions '((t a-on-table) (t c-clear) (t a-clear))
;;                :effects '((nil a-on-table) (nil c-clear) (t a-on-c)))
;;    (make-operator :name 'b-table-to-a
;;                :preconditions '((t b-on-table) (t a-clear) (t b-clear))
;;                :effects '((nil b-on-table) (nil a-clear) (t b-on-a)))
;;    (make-operator :name 'b-table-to-c
;;                :preconditions '((t b-on-table) (t c-clear) (t b-clear))
;;                :effects '((nil b-on-table) (nil c-clear) (t b-on-c)))
;;    (make-operator :name 'c-table-to-a
;;                :preconditions '((t c-on-table) (t a-clear) (t c-clear))
;;                :effects '((nil c-on-table) (nil a-clear) (t c-on-a)))
;;    (make-operator :name 'c-table-to-b
;;                :preconditions '((t c-on-table) (t b-clear) (t c-clear))
;;                :effects '((nil c-on-table) (nil b-clear) (t c-on-b)))
;;    ;; move to table operators
;;    (make-operator :name 'a-b-to-table
;;                :preconditions '((t a-on-b) (t a-clear))
;;                :effects '((t a-on-table) (nil a-on-b) (t b-clear)))
;;    (make-operator :name 'a-c-to-table
;;                :preconditions '((t a-on-c) (t a-clear))
;;                :effects '((t a-on-table) (nil a-on-c) (t c-clear)))
;;    (make-operator :name 'b-a-to-table
;;                :preconditions '((t b-on-a) (t b-clear))
;;                :effects '((t b-on-table) (nil b-on-a) (t a-clear)))
;;    (make-operator :name 'b-c-to-table
;;                :preconditions '((t b-on-c) (t b-clear))
;;                :effects '((t b-on-table) (nil b-on-c) (t c-clear)))
;;    (make-operator :name 'c-a-to-table
;;                :preconditions '((t c-on-a) (t c-clear))
;;                :effects '((t c-on-table) (nil c-on-a) (t a-clear)))
;;    (make-operator :name 'c-b-to-table
;;                :preconditions '((t c-on-b) (t c-clear))
;;                :effects '((t c-on-table) (nil c-on-b) (t b-clear)))
;;    ;; block-to-block operators
;;    (make-operator :name 'a-b-to-c
;;                :preconditions '((t a-on-b) (t a-clear) (t c-clear))
;;                :effects '((nil a-on-b) (t a-on-c) (nil c-clear) (t b-clear)))
;;    (make-operator :name 'a-c-to-b
;;                :preconditions '((t a-on-c) (t a-clear) (t b-clear))
;;                :effects '((nil a-on-c) (t a-on-b) (nil b-clear) (t c-clear)))
;;    (make-operator :name 'b-a-to-c
;;                :preconditions '((t b-on-a) (t b-clear) (t c-clear))
;;                :effects '((nil b-on-a) (t b-on-c) (nil c-clear) (t a-clear)))
;;    (make-operator :name 'b-c-to-a
;;                :preconditions '((t b-on-c) (t b-clear) (t a-clear))
;;                :effects '((nil b-on-c) (t b-on-a) (nil a-clear) (t c-clear)))
;;    (make-operator :name 'c-a-to-b
;;                :preconditions '((t c-on-a) (t c-clear) (t b-clear))
;;                :effects '((nil c-on-a) (t c-on-b) (nil b-clear) (t a-clear)))
;;    (make-operator :name 'c-b-to-a
;;                :preconditions '((t c-on-b) (t c-clear) (t a-clear))
;;                :effects '((nil c-on-b) (t c-on-a) (nil a-clear) (t b-clear))))
;;   "A list of strips operators without their uniq gensyms set yet -- 
;; doesn't matter really -- but NOT including a goal or start operator")

;; (defparameter *start-effects*
;;   ;; Sussman Anomaly
;;   '((t a-on-table) (t b-on-table) (t c-on-a) (t b-clear) (t c-clear))
;;   "A list of predicates which specify the initial state")

;; (defparameter *start-effects*
;;   ;; another simple situation: all on table
;;   '((t a-on-table) (t a-clear)
;;     (t b-on-table) (t b-clear)
;;     (t c-on-table) (t c-clear))) 

;; (defparameter *goal-preconditions*
;;   '((t a-on-b) (t b-on-c) (t c-on-table) (t a-clear)))




;;;; An Solution to 3-BLOCK-WORLD, Sussman Anomaly, in SBCL:


;; * (do-pop)
;;
;; Search Depth: 1
;; Search Depth: 2
;; Search Depth: 3
;; Search Depth: 4
;; Search Depth: 5
;; Search Depth: 6
;; Search Depth: 7
;; Search Depth: 8
;; Search Depth: 9
;; Search Depth: 10
;; Search Depth: 11
;; Search Depth: 12
;; Search Depth: 13
;; Solution Discovered:
;;
;; #< PLAN operators: 
;; #S(OPERATOR
;;    :NAME B-TABLE-TO-C
;;    :UNIQ G251257
;;    :PRECONDITIONS ((T B-ON-TABLE) (T C-CLEAR) (T B-CLEAR))
;;    :EFFECTS ((NIL B-ON-TABLE) (NIL C-CLEAR) (T B-ON-C)))
;; #S(OPERATOR
;;    :NAME A-TABLE-TO-B
;;    :UNIQ G225453
;;    :PRECONDITIONS ((T A-ON-TABLE) (T B-CLEAR) (T A-CLEAR))
;;    :EFFECTS ((NIL A-ON-TABLE) (NIL B-CLEAR) (T A-ON-B)))
;; #S(OPERATOR
;;    :NAME C-A-TO-TABLE
;;    :UNIQ G182393
;;    :PRECONDITIONS ((T C-ON-A) (T C-CLEAR))
;;    :EFFECTS ((T C-ON-TABLE) (NIL C-ON-A) (T A-CLEAR)))
;; #S(OPERATOR
;;    :NAME START
;;    :UNIQ G616
;;    :PRECONDITIONS NIL
;;    :EFFECTS ((T A-ON-TABLE) (T B-ON-TABLE) (T C-ON-A) (T B-CLEAR) (T C-CLEAR)))
;; #S(OPERATOR
;;    :NAME GOAL
;;    :UNIQ G617
;;    :PRECONDITIONS ((T B-ON-C) (T A-ON-B) (T A-CLEAR) (T C-ON-TABLE))
;;    :EFFECTS NIL) 
;; links: 
;; #< (G616)START -> (G251257)B-TABLE-TO-C : (T C-CLEAR) >
;; #< (G616)START -> (G251257)B-TABLE-TO-C : (T B-CLEAR) >
;; #< (G616)START -> (G225453)A-TABLE-TO-B : (T B-CLEAR) >
;; #< (G182393)C-A-TO-TABLE -> (G225453)A-TABLE-TO-B : (T A-CLEAR) >
;; #< (G616)START -> (G182393)C-A-TO-TABLE : (T C-CLEAR) >
;; #< (G182393)C-A-TO-TABLE -> (G617)GOAL : (T A-CLEAR) >
;; #< (G616)START -> (G251257)B-TABLE-TO-C : (T B-ON-TABLE) >
;; #< (G616)START -> (G225453)A-TABLE-TO-B : (T A-ON-TABLE) >
;; #< (G616)START -> (G182393)C-A-TO-TABLE : (T C-ON-A) >
;; #< (G251257)B-TABLE-TO-C -> (G617)GOAL : (T B-ON-C) >
;; #< (G225453)A-TABLE-TO-B -> (G617)GOAL : (T A-ON-B) >
;; #< (G182393)C-A-TO-TABLE -> (G617)GOAL : (T C-ON-TABLE) > 
;; orderings: 
;; #[ (G251257)B-TABLE-TO-C -> (G225453)A-TABLE-TO-B ]
;; #[ (G182393)C-A-TO-TABLE -> (G225453)A-TABLE-TO-B ]
;; #[ (G182393)C-A-TO-TABLE -> (G251257)B-TABLE-TO-C ]
;; #[ (G251257)B-TABLE-TO-C -> (G617)GOAL ]
;; #[ (G616)START -> (G251257)B-TABLE-TO-C ]
;; #[ (G225453)A-TABLE-TO-B -> (G617)GOAL ]
;; #[ (G616)START -> (G225453)A-TABLE-TO-B ]
;; #[ (G182393)C-A-TO-TABLE -> (G617)GOAL ]
;; #[ (G616)START -> (G182393)C-A-TO-TABLE ]
;; #[ (G616)START -> (G617)GOAL ]
;; >







;;;;; COMPILATION WARNINGS

;; You should have few warnings from compilation, all of them spurious.  These
;; are due entirely to missing functions (because they're defined later in the file)
;; and undeclared global variables (because they're defined later in the file).
;; SBCL complains about these:

;; in: DEFSTRUCT LINK
;; caught STYLE-WARNING:
;;   undefined function: PRINT-LINK
;; 
;; in: DEFSTRUCT PLAN
;; caught STYLE-WARNING:
;;   undefined function: PRINT-PLAN
;; 
;; in: DEFUN PICK-PRECOND
;; caught STYLE-WARNING:
;;   undefined function: NUM-ALL-OPERATORS
;; 
;; in: DEFUN ALL-OPERATORS
;; caught WARNING:
;;   undefined variable: *OPERATORS-FOR-PRECOND*
;; 
;; in: DEFUN NUM-ALL-OPERATORS
;; caught WARNING:
;;   undefined variable: *NUM-OPERATORS-FOR-PRECOND*
;; 
;; in: DEFUN SELECT-SUBGOAL
;; caught STYLE-WARNING:
;;   undefined function: CHOOSE-OPERATOR
;; 
;; in: DEFUN CHOOSE-OPERATOR
;; caught STYLE-WARNING:
;;   undefined function: ADD-OPERATOR
;; caught STYLE-WARNING:
;;   undefined function: HOOK-UP-OPERATOR
;; 
;; in: DEFUN HOOK-UP-OPERATOR
;; caught STYLE-WARNING:
;;   undefined function: RESOLVE-THREATS
;; caught STYLE-WARNING:
;;   undefined function: THREATS
;; 
;; in: DEFUN ALL-PROMOTION-DEMOTION-PLANS
;; caught STYLE-WARNING:
;;   undefined function: DEMOTE
;; caught STYLE-WARNING:
;;   undefined function: PROMOTE
;; 
;; in: DEFUN BUILD-OPERATORS-FOR-PRECOND
;; caught WARNING:
;;   undefined variable: *OPERATORS*
;; 
;; in: DEFUN DO-POP
;; caught WARNING:
;;   undefined variable: *GOAL-PRECONDITIONS*
;; caught WARNING:
;;   undefined variable: *START-EFFECTS*

;; ... I really oughta rearrange the code so these go away.  :-)