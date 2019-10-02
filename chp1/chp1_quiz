;; Chp1 Quiz

;;(BK-IZER): This function takes in anything and produces:

;;'M if the argument is not a list
;;'B if the list contains at least three elements (at the top level)
;;'K when the list contains at most 5 atoms anywhere inside it (including in nested lists)
;;A list containing the results of running BK-IZER on the contents of the input list if none of the above apply.

(Declaim (Optimize (debug 3)))
(ql:quickload :lisp-unit)
(use-package :lisp-unit)
(defparameter *print-failures* t)

(defun count-items (nested)
  (cond ((null nested) 0)
        ((atom (first nested)) (+ 1 (count-items (rest nested))))
        (t (+ (count-items (first nested))
              (count-items (rest nested))))))

(defun bk-izer (item)
  (cond ((not (listp item)) 'M)
        ((> (length item) 2) 'B)
        ((< (count-items item) 6) 'K)
        (t (mapcar #'bk-izer item))))