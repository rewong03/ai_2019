(defun flatten (lst)
  (cond ((null lst)
         '())
        ((listp (first lst))
         (append (flatten (first lst))
                 (flatten (rest lst))))
        (t
         (append (list (first lst))
                 (flatten (rest lst))))))


(defun gerbil (name activity)
  (flatten `(The gerbil ,name loves to ,activity)))

(defun dog (name
            &optional (barks 0))
  (if (<= barks 0)
      (flatten `(,name loves to bark.))
      (flatten (append `(,name loves to bark. )
                       (loop for x from 1 to barks
                             collect '(Bark! ))))))

(defun cat (name)
  (let ((actions '(purr meow raawr)))
    (flatten (append `(The cat ,name loves to )
                     (list (elt actions (random 3)))))))

(defun add (&rest lst)
  (apply #'+ lst))

(defun multiply (&rest lst)
  (apply #'* lst))

(defun random-choice (&rest lst)
  (elt lst (random (length lst))))

(defparameter *ops*
  `((?gerbil . ,#'gerbil)
    (?dog . ,#'dog)
    (?cat . ,#'cat)
    (?+ . ,#'add)
    (?* . ,#'multiply)
    (?random . ,#'random-choice)))

(defun run-pat (pat)
  (let ((pattern (first pat))
        (args (rest pat)))
    (apply (cdr (assoc pattern *ops*))
           args)))
