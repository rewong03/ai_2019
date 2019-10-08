;; Chp3 HW 2

;; Write a function that returns true if the inputted number is one more than a multiple of 8
(defun one-more-mult (num)
  (= (mod num 8)
     1))

;; Given a list of integers, return all of the elements that are either greater than 100 or leave a remainder of 6 when divided by 17.
(defun all-6-by-17 (num-list)
  (loop for num in num-list
        when (or (> num 100)
                 (= (mod num 17) 6)) collect num))

;; Given a list of words, return the number of words in the list that are longer than 6 letters.
(defun long-words (word-list)
  (loop for word in word-list
        when (> (length word) 6) collect word))

;;Given a list of lists ys, if a sublist y of ys begins with the symbol GOOD, then put every element from the list y in the answer.
(defun good-lists (ys)
  (loop for y in ys
        when (equal (first y)
                    'good) append y))

;; In all of the following problems, you are given a list of ordered pairs of numbers

;; Return a list of all of the x values that are in the interval [0,10].
(defun x010 (pts)
  (loop for point in pts
        when (and (>= (first point) 0)
                  (<= (first point) 10)) collect point))

;; Return a list of all of the points whose y values are either greater than 200 or less than -200.
(defun y200 (pts)
  (loop for point in pts
        when (and (>= (second point) -200)
                  (<= (second point) 200)) collect point))

;; Find the greatest value of f(x,y) = x^2 + 3y^2 - 2xy using the points in a list.
(defun  ptf (pts)
  (loop for point in (mapcar #'(lambda (x) (+ (expt (first x) 2)
                                              (* 3 (expt (second x) 2))
                                              (- (* 2 (first x) (second x)))))
                             pts)
        maximize point))

;; Find the smallest difference |x - y| from a list.
(defun smd (pts)
  (loop for point in (mapcar #'(lambda (x) (abs (- (first x)
                                                   (second x))))
                             pts)
        minimize point))

;; If every point is on the parabola y = x^2 return true.
(defun aop (pts)
  (every #'(lambda (x) x)
         (mapcar #'(lambda (point) (= (second point)
                                      (expt (first point) 2)))
                 pts)))

;; If any point has |y - x^2| > 10 return true.
(defun is-far (pts)
  (loop for point in pts
        when (> (abs (- (second point) (expt (first point) 2)))
                10) do (return t)))

;; If no point in the list has y = 10^x return true.
(defun not10x (pts)
  (every #'(lambda (x) (not x))
         (mapcar #'(lambda (point) (= (second point)
                                      (expt 10 (first point))))
                 pts)))

;; Given a list of triples (x,y,z), return a (list x y) for every point where z = x^2 + y^2
(defun xyzTrip (trip)
  (loop for point in trip
        when (= (third point)
                (+ (expt (first point) 2)
                   (expt (second point) 2))) collect (list (first point) (second point))))
