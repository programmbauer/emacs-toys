;;; -*- lexical-binding: t; -*-
;; This is a minimalistic lisp interpreter written in lisp inspired by
;; William Byrd's talk "The Most Beautiful Program Ever Written"
;; (https://www.youtube.com/watch?v=OyfBQmvr2Hc). In order to make the
;; distinction between the interpreted language and the implementation
;; language more obvious, I have chosen different keywords than in
;; Emacs Lisp (defun is def, t is true, eq is =, ...). See at the end
;; of the file for more information.

;;Step 1: Define an interpreter 
(defun t1-eval (expr env)
  (pcase expr
    (`(def ,var ,val)
     (let ((evalval (t1-eval val env)))
       (setq global
	     (lambda (x) (if (eq x var) evalval (funcall env x)))))
     var)
    (`(if ,if ,th ,el)
     (if (equal (t1-eval if env) 'false) (t1-eval el env) (t1-eval th env)))
    (`(= ,x ,y)
     (if (equal (t1-eval x env) (t1-eval y env)) 'true 'false))
    (`',x  x)
    ;; Symbols, lambdas and function application, see W.E. Byrd
    ((pred symbolp) (funcall env expr))
    (`(fun (,x) ,body) (lambda (arg)
			(t1-eval body
				 (lambda (y)
				   (if (eq x y) arg (funcall env y))))))
    (`(,rator ,rand) (funcall (t1-eval rator env)
			      (t1-eval rand  env)))
    (_ (message "t1-eval -- Invalid expression: %s" expr))))

;;Step 2: Define a global environment
(defvar global nil)
(setq global (lambda (x) (message "t1-eval --  Unknown identifier: %s" x)))
;;Step 2.5: Helper function
(defun printlist (l)
  (if (symbolp l)
      (symbol-name l)
    (format "(%s %s)"
	    (printlist (funcall l (lambda (p) (lambda (q) p))))
	    (printlist (funcall l (lambda (p) (lambda (q) q)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Step 3: Evaluate some function definitions
;;Lists
(t1-eval '(def nil 'nil) global)

(t1-eval '(def cons  (fun (x) (fun (y) (fun (m) ((m x) y))))) global)
(t1-eval '(def first (fun (z) (z (fun (p) (fun (q) p))))) global)
(t1-eval '(def rest  (fun (z) (z (fun (p) (fun (q) q))))) global)

(t1-eval '(def empty (fun (l) (= l 'nil))) global)
(t1-eval '(def second (fun (z) (first (rest z)))) global)

;;Y combinator (emulate recursion)
(t1-eval
 '(def Y
    (fun (h)
	 ((fun (x) (x x))
	  (fun (g)
	       (h (fun (arg) ((g g) arg)))))))
 global)

;;Map
(t1-eval
 '(def map
    (Y (fun (f)
	    (fun (mapfun) (fun (l)
			       (if (= nil l)
				   nil
				 ((cons (mapfun (first l)))
				  ((f mapfun) (rest l)))))))))
 global)

;;Church numerals
(t1-eval '(def zero nil) global)
(t1-eval '(def succ (fun (number) ((cons '1+) number))) global)

(t1-eval '(def evenness
	    (Y (fun (f)
		    (fun (try)
			 (fun (l)
			      (if (= try 'even)
				  (if (= l nil)
				      'true
				    ((f 'odd) (rest l)))
				(if (= l nil)
				    'false
				  ((f 'even) (rest l)))))))))
    global)
(t1-eval '(def even (fun (l) ((evenness 'even) l))) global)
(t1-eval '(def odd  (fun (l) ((evenness 'odd)  l))) global)
(t1-eval '(def plus
	    (Y (fun (f)
		    (fun (x)
			 (fun (y)
			      (if (= nil y)
				  x
				((f (succ x)) (rest y))))))))
	 global)



;;Step 4: Test cases
(require 'ert)
(ert-deftest test-cons ()
 (should (equal (t1-eval '(second ((cons 'x) ((cons 'y) nil))) global) 'y)))
(ert-deftest test-map ()
  (should (equal (t1-eval '(second ((map (fun (x) 'yay))  ((cons 'x) ((cons 'y) ((cons 'z) nil))))) global) 'yay))
  (should (equal (t1-eval '(second ((map first)  ((cons ((cons 'x) nil)) ((cons ((cons 'y) nil)) ((cons ((cons 'z) nil)) nil))))) global) 'y)))
(ert-deftest test-evenness ()
  (should (equal (t1-eval '((evenness 'odd) ((cons 'x) ((cons 'y) nil))) global) 'false))
  (should (equal (t1-eval '(even nil) global) 'true))
  (should (equal (t1-eval '(odd nil)  global) 'false))
  (should (equal (t1-eval '(even ((cons 'y) nil)) global) 'false))
  (should (equal (t1-eval '(odd  ((cons 'y) nil)) global) 'true))
  (should (equal (t1-eval '(even ((cons 'x) ((cons 'y) nil))) global) 'true))
  (should (equal (t1-eval '(odd  ((cons 'x) ((cons 'y) nil))) global) 'false))
  (should (equal (t1-eval '((evenness 'even) (succ (succ zero))) global) 'true)))
(ert-deftest test-numerals-and-printlist ()
  (should (equal (printlist (t1-eval '((plus zero) zero) global)) "nil"))
  (should (equal (printlist (t1-eval '((plus (succ (succ zero))) (succ (succ zero))) global)) "(1+ (1+ (1+ (1+ nil))))"))
  (should (equal (printlist (t1-eval '((cons ((cons 'x) nil)) ((cons ((cons 'y) nil)) ((cons ((cons 'z) nil)) nil))) global)) "((x nil) ((y nil) ((z nil) nil)))")))
(message "'done")

;; Step 5: Documentation
;; This is a simple evaluator for expressions in a simple lisp
;; dialect.  To evaluate an expression in the global environment,
;; execute the following code: (t1-eval 'yourexpression global). The
;; interpreter currently suppports the following primitives:
;;; - (def expr name) Bind the result of evaluating <expr> to <name>
;;; - (if cond then else) if evaluating <cond> results in anything
;;;   other than 'false, evaluate <then>. Otherwise evaluate <else>.
;;; - (= x y) if <x> is equal to <y>, return 'true. Else retuen 'false
;;; - 'x      return x without evaluating it
;;; - x       return the value bound to the symbol <x>
;;; - (fun (x) y) return a lambda, i.e. a function that returns the
;;;   value of <y> after replacing all occurrences of <x> by the
;;;   provided argument
;;; - (x y) Applies the function (bound to the name) <x> to the
;;;   result of evaluating <y>
;; Additionally, several convenience functions have been defined:
;;; - ((cons x) y) returns a pair <z> of <x> and <y>; z = (x y)
;;; - (first z) returns the first component of a pair <z>
;;; - (rest z) returns the second component of a pair <z>
;;; - lists can be constructed as nested pairs (x1 (x2 (x3 nil)))
;;; - the constant <nil> should be used for empty lists
;;; - (empty x) returns 'true if x is equal to 'nil
;;; - (second x) returns the second element of a list
;;; - ((map f) l) returns a list of the results of applying <f> to
;;;   each element of the list <l>
;;; - zero is equivalent to <nil>. It should be used as the number 0
;;; - (succ x) returns the list (1+ x). This should be used as the
;;;   successor of the number <x>. -> Peano arithmetic/Church encoding
;;; - (even x) returns 'true if x is an even number, 'false otherwise
;;; - (odd x) returns 'true if x is an odd number, 'false otherwise
;;; - ((plus x) y) returns the sum of the numbers <x> and <y>
;;; - (Y f) is the y combinator. It allows the definition of
;;;   quasi-recursive functions
