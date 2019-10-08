
(defparameter *CRAPS-RULES* '((0 7 -1)
                              (0 11 -1)
                              (0 2 1)
                              (0 3 1)
                              (0 12 1)
                              (0 point 0)
                              (point point 1)
                              (point 7 -1)
                              (point x 0)))

(defun replace-constant (rule constant var)
  (loop for val in rule
        if (equal constant val)
        collect var
        else
        collect val))

(defun rule-true (rule point roll)
  (and (if (symbolp (first rule))
           (setf rule (replace-constant rule (first rule) point))
           t)
       (if (symbolp (second rule))
           (setf rule (replace-constant rule (second rule) roll))
           t)
       (and (= (first rule)
               point)
            (= (second rule)
               roll))))

(defun simulate-1 (point roll)
  (loop for rule in *craps-rules*
        when (rule-true rule point roll) return (third rule)))

(defun simulate ()
  (let ((first-roll (+ (+ (random 6) 1)
                       (+ (random 6) 1))))
    (if (= 0 (simulate-1 0 first-roll))
        (cons (list 0 first-roll (simulate-1 0 first-roll))
                (loop with point = first-roll
                      with roll = (+ (+ (random 6) 1)
                                     (+ (random 6) 1))
                      with is-won = (simulate-1 point roll)
                      collect (list point roll is-won)
                      while (= is-won 0)
                      do (setf roll (+ (+ (random 6) 1)
                                       (+ (random 6) 1)))
                      do (setf is-won (simulate-1 point roll))))
        (list 0 first-roll (simulate-1 0 first-roll)))))
