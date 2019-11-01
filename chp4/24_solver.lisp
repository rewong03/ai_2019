(defparameter *ops* '(+ - * /))

(defun insert-before (n item lst)
  (loop for x from 0 to (- (length lst) 1)
        if (= n x)
        collect item
        else
        collect (elt lst x)))

(defun permute (lst)
  (cond ((= (length lst) 1)
         (list lst))
        (t
         (loop for y in (permute (rest lst))
               collect (loop for n from 0 to (- (length y) 1)
                             collect (insert-before n (first lst) y))))))
