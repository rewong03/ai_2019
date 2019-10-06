(Declaim (Optimize (debug 3)))
(ql:quickload :lisp-unit)
(use-package :lisp-unit)
(defparameter *print-failures* t)
;; Chp1 Problems
;; Warmups
;; 1. GREET-IZE: String -> (String->String). Take in a greeting. Return a function that takes in a name and outputs the string “greeting, name”.
(defun greet-ize (greeting)
  (lambda (name) (concatenate 'string greeting ", " name)))

(define-test TEST-GREET-IZE 
  "Verify you understand function-returning functions."
  (assert-equal "Hello, Doc" (funcall (greet-ize "Hello") "Doc"))
  (assert-equal "Goodbye, World" (funcall (greet-ize "Goodbye") "World")))

;; 2. WIPE-NESTED: List -> List. Change every non-list any nested list of the input to ‘BLACK.
(defun wipe-nested (list)
  (cond ((null list) ())
        ((listp (first list)) (append (list (wipe-nested (first list)))
                                      (wipe-nested (rest list))))
        (t (append '(Black)
                   (wipe-nested (rest list))))))

(define-test TEST-WIPE
  "Make sure the structure of the list is retained when wiping."
  (assert-equal '(BLACK BLACK BLACK) (wipe-nested '(orange blue green)))
  (assert-equal '(BLACK (BLACK BLACK BLACK) BLACK ((BLACK)))
                (Wipe-Nested '(R (R G B) G ((B))))))

;; HW
;; 1. Function DUBL: doubles its input.
(defun dubl (num)
  (* 2 num))

;; 2. Write a single test for DBL.
(define-test TEST-DUBL
  (assert-equal 200 (dubl 100)))

;; 3. Function DUBLR: doubles every input in a list.
(defun dublr (list)
  (mapcar #'dubl list-tags))

;; 4. Colorizer: turns integers into color symbols: 1 -> 'RED', 2 -> 'WHITE, 10 -> 'BLUE, any other becomes 'NONE.
(defun colorizer (num)
  (cond ((= num 1) 'RED)
        ((= num 2) 'WHITE)
        ((= num 10) 'BLUE)
        (t 'NONE)))

;; 5. Function N-NER: takes in a number and a list. Adds the number to every element in the list (returning the new list).
(defun n-ner (list num)
  (cond ((null list) list)
        (t (append (list (+ (first list) num))
                   (n-ner (rest list) num)))))

;; 6. Function MY-MAKE-SEQ: inputs are three numbers (a,b,c), output is a function that takes in a number x and puts out the list (x+a, x+b, x+c).
(defun my-make-seq (a b c)
  (lambda (x) (n-ner (list a b c) x)))

;; 7. Write two tests for MY-MAKE-SEQ.
(define-test TEST-MY-MAKE-SEQ
  (assert-equal '(2 3 4) (funcall (my-make-seq 1 2 3) 1))
  (assert-equal '(5 7 9) (funcall (my-make-seq 3 5 7) 2)))