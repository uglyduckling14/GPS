;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Module: ops.lisp
;;; different worlds and operators for the GPS planner.
;;; bugs to vladimir kulyukin in canvas
;;; sussman anomaly
;;; ((START)
;;; (EXECUTE PUT-C-FROM-A-ON-T)
;;; (EXECUTE PUT-B-FROM-T-ON-C)
;;; (EXECUTE PUT-A-FROM-T-ON-B))
;;; monkey/banana
;;; ((START)
;;;(EXECUTE PUSH-CHAIR-TO-MIDDLE)
;;; (EXECUTE CLIMB-ON-CHAIR)
;;; (EXECUTE DROP-BALL)
;;; (EXECUTE GRASP-BANANAS)
;;; (EXECUTE EAT-BANANAS))


;;; =========================================

(in-package :user)

(defstruct op "An GPS operator"
  (action nil) 
  (preconds nil) 
  (add-list nil) 
  (del-list nil))

(defun executing-p (x)
  "Is x of the form: (execute ...) ?"
  (starts-with x 'execute))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'execute (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
    (make-op :action action :preconds preconds
             :add-list add-list :del-list del-list)))

;;; ================= Son At School ====================

(defparameter *school-world* '(son-at-home car-needs-battery
					   have-money have-phone-book))

(defparameter *school-ops*
  (list
    ;;; operator 1
   (make-op :action 'drive-son-to-school
	    :preconds '(son-at-home car-works)
	    :add-list '(son-at-school)
	    :del-list '(son-at-home))
   ;;; operator 2
   (make-op :action 'shop-installs-battery
	    :preconds '(car-needs-battery shop-knows-problem shop-has-money)
	    :add-list '(car-works))
   ;;; operator 3
   (make-op :action 'tell-shop-problem
	    :preconds '(in-communication-with-shop)
	    :add-list '(shop-knows-problem))
   ;;; operator 4
   (make-op :action 'telephone-shop
	    :preconds '(know-phone-number)
	    :add-list '(in-communication-with-shop))
   ;;; operator 5
   (make-op :action 'look-up-number
	    :preconds '(have-phone-book)
	    :add-list '(know-phone-number))
   ;;; operator 6
   (make-op :action 'give-shop-money
	    :preconds '(have-money)
	    :add-list '(shop-has-money)
	    :del-list '(have-money))))

;;; ================= Sussman's Anomaly ====================

(defparameter *blocks-world* '(a-on-t b-on-t c-on-a clear-c clear-b))

(defparameter *blocks-ops*
  (list
    (make-op :action 'put-a-from-t-on-b
             :preconds '(a-on-t clear-a clear-b)
             :add-list '(a-on-b)
             :del-list '(a-on-t clear-b))
    (make-op :action 'put-c-from-a-on-t
             :preconds '(c-on-a clear-c)
             :add-list '(c-on-t clear-a)
             :del-list '(c-on-a))
    (make-op :action 'put-b-from-t-on-c
             :preconds '(b-on-t clear-b clear-c)
             :add-list '(b-on-c)
             :del-list '(b-on-t clear-c))
    (make-op :action 'put-c-from-t-on-b
             :preconds '(c-on-t clear-c clear-b)
             :add-list '(c-on-b)
             :del-list '(c-on-t clear-b))))

    
	    
;;; ================= Monkey and Bananas ====================

(defparameter *banana-world* '(at-door on-floor has-ball hungry chair-at-door))

(defparameter *banana-ops*
  (list
    (make-op :action 'push-chair-to-middle
             :preconds '(at-door chair-at-door)
             :add-list '(chair-at-middle at-middle)
             :del-list '(at-door chair-at-door))
    (make-op :action 'climb-on-chair
             :preconds '(at-middle on-floor chair-at-middle)
             :add-list '(on-chair)
             :del-list '(on-floor))
    (make-op :action 'drop-ball
             :preconds '(has-ball)
             :add-list '(hands-free)
             :del-list '(has-ball))
    (make-op :action 'grasp-bananas
             :preconds '(on-chair hands-free)
             :add-list '(has-bananas)
             :del-list '(hands-free))
    (make-op :action 'eat-bananas
             :preconds '(has-bananas hungry)
             :add-list '(not-hungry)
             :del-list '(hungry has-bananas))))
  
(mapc #'convert-op *school-ops*)
(mapc #'convert-op *blocks-ops*)
(mapc #'convert-op *banana-ops*)

(provide :ops)
