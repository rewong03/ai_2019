;; Chp3 HW1

;; 1. Write a function CONSTRAIN that takes three inputs called X, MAX, and MIN. If X is less than MIN, it should return MIN; if X is greater than MAX, it should return MAX.
;;    Otherwise, since X is between MIN and MAX, it should return X.
(defun constrain (x min max)
  (cond ((< x min)
         min)
        ((> x max)
         max)
        (t
         x)))
;; 2. Write the function MEAN that finds the arithmetic mean of a nonempty list of numbers.
(defun mean (num-list)
  (let ((sum 0))
    (dolist (number num-list)
      (incf sum number))
    (/ sum (length num-list))))
;; 3. Calculate standard dev of a list of numbers.
(defun stdev (num-list)
  (let ((sum-square-dif 0) (list-mean (mean num-list)))
    (dolist (number num-list)
      (incf sum-square-dif
             (expt (- number list-mean) 2)))
    (sqrt (/ sum-square-dif
             (length num-list)))))
;; 4. Write a function HOWCOMPUTE. HOWCOMPUTE takes three numbers as input and figures out what operation would produce the third from the first two. Possible answers: ‘ADD ‘SUBTRACT ‘MULTIPLY and ‘BEATS-ME.
(defun howcompute (a b c)
  (cond ((= c (* a b))
         'multiply)
        ((= c (+ a b))
         'add)
        ((or (= c (- a b))
             (= c (- b a)))
         'subtract)
        (t 'beats-me)))
;; 5. Write a function that referees rock paper scissors.
(defun play (p1 p2)
  (cond ((equal p1 p2)
         0)
        ((or (and (equal p1 'scissors)
                  (equal p2 'rock))
             (and (equal p1 'rock)
                  (equal p2 'paper))
             (and (equal p1 'paper)
                  (equal p2 'scissors)))
         )))

