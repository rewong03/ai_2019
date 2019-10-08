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
;; 6. A palindrome is a sequence that reads the same forwards and backwards. The list (A B C D C B A) is a palindrome; (A B C A B C) is not. Write a function palindromep that returns T if its input is a palindrome.
(defun palindromep (palindrome)
  (equal palindrome (reverse palindrome)))

;; 7. Write make-palindrome that takes in a list and makes a palindrome out of it in a simple way. Given (YOU AND ME) as input it should return (YOU AND ME ME AND YOU).
(defun make-palindrome (not-palindrome)
  (append not-palindrome (reverse not-palindrome)))

;; 8. Write a crude program to compute the probability that an integer picked from M to N is a perfect square.
(defun perfect-square (m n)
  (let ((num-squares 0))
    (dotimes (x (- (+ n 1) m) num-squares)
      (when (= (sqrt (+ m x)) (floor (sqrt (+ m x))))
        (incf num-squares)))))

;; 9.
;; Game 1: Roll a die 4 times, if you roll a 6 at any time you win.
(defun game1 ()
  (dotimes (x 4)
    (when (= (+ (random 6) 1) 6)
      (return t))))
;; Game 2: Roll a pair of die 24 times. If you roll 2 sixes you win.
(defun game2 ()
  (dotimes (x 24)
    (when (and (= (+ (random 6) 1) 6)
               (= (+ (random 6) 1) 6))
      (return t))))
;; run it with 100,000 tries at each game to estimate the probability of winning. Report which game is easier to win, and the difference in the estimated probability of winning when compared to the other game.
(defun run-game1 ()
  (let ((won 0))
    (dotimes (x 100000)
      (when (game1)
        (incf won)))
    (/ won 100000)))
(defun run-game2 ()
  (let ((won 0))
    (dotimes (x 100000)
      (when (game2)
        (incf won)))
    (/ won 100000)))
(defun calc-prob ()
  "Calculates the difference in probability of winning game1 to game2"
  (- (run-game1) (run-game2)))

;; 10. Check whether two sets are equal
(defun set-equal (s1 s2)
  (subsetp s1 s2))

;; 11. We are going to write a program that compares the descriptions of two objects and tells how many features they have in common. The descriptions will be represented as a list of features, with the symbol -VS- separating the first object from the second.
(defun left-hand (symbols)
  (loop for i from 0 to (length symbols)
        while (not (equal (elt symbols i)
                          '-vs-))
        collect (elt symbols i)))

(defun right-hand (symbols)
  (reverse (left-hand (reverse symbols))))

(defun count-common (symbols)
  (let ((common 0) (s1 (left-hand symbols)) (s2 (right-hand symbols)))
    (loop for item in s1
          do (when (member item s2)
               (incf common))
          finally (return common))))

(defun compare (symbols)
  (list (count-common symbols) 'common 'features))
