;; Chp1 Bookwork

;; Write a function to exponentiate, or raise a number to an integer power.
(defun power (base exponent)
  (cond ((= exponent 0)
         1)
        (t
         (* base (power base (- exponent 1))))))

(defun count-atoms (items)
  (cond ((null items)
         0)
        ((listp (first items))
         (+ (count-atoms (first items))
            (count-atoms (rest items))))
        (t (+ (count-atoms (rest items))
              1))))

(defun count-anywhere (atom list)
  (cond ((null list)
         0)
        ((equal atom
                (first list))
         (+ (count-anywhere atom (rest list))
            1))
        ((listp (first list))
         (+ (count-anywhere atom (first list))
            (count-anywhere atom (rest list))))
        (t
         (count-anywhere atom (rest list)))))
