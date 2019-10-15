;; Chp4 HW1

;; 1. (ball)
;; Make a structure ball that takes a symbol as a name and a number as the circumference.
(defstruct ball
  type
  circ)

;; Make a softball with circumference 12.
(defvar softball
  (make-ball :type 'softball
             :circ 12))

;; Modify the circumference to be 13.
(setf (ball-circ softball) 13)

;; Write a function that takes in anything (even lists and numbers) and returns true if the item is a ball of circumference greater than 10.
(defun circ10 (item)
  (if (ball-p item)
      (> (ball-circ item) 10)
      nil))

;; 2. The function pos+ takes in a list and returns a list of each element plus its position in the list.
;; i. Recursion
(defun helpPos+ (num-list)
  (if num-list
      (cons (+ (first num-list)
               (- (length num-list) 1))
            (helpPos+ (rest num-list)))
      num-list))

(defun recurPos+ (num-list)
  (reverse (helpPos+ (reverse num-list))))

;; ii. Iteration
(defun iterPos+ (num-list)
  (loop for x from 0 to (- (length num-list) 1)
        collect (+ x
                   (nth x num-list))))

;; iii. Mapcar
(defun mapPos+ (num-list)
  (mapcar #'+ num-list
          (loop for x from 0 to (- (length num-list) 1)
                collect x)))

;; 3. Write a function rle that takes in a list and outputs a list of pairs (item repeats), where repeats tells how many times in a row the item is repeated.
(defun countNum (item item-list)
  "Counts amount of items in a row for single item"
  (cond ((null item-list)
         0)
        ((not (equal item (first item-list)))
         0)
        (t
         (+ (countNum item (rest item-list))
            1))))

(defun drop (n items)
  (if (= n 0)
      items
      (drop (- n 1) (rest items))))

(defun rle (item-list)
  (if (null item-list)
      nil
      (let ((item-count (countNum (first item-list) item-list)))
        (cons (list (first item-list)
                    item-count)
              (rle (drop item-count item-list))))))

;; 4. (div2) You will write the function div2, divides a number by two. It has a keyword argument :skip-odd which defaults to false (nil).
(defun div2 (num &key skip-odd)
  (if (and skip-odd
           (= (mod num 2) 1))
      num
      (/ num 2)))

;; 5. (add2len) Use &rest to make a function that returns the first number added to the number of arguments it receives.
(defun add2len (num &rest num-list)
  (+ num (length num-list)))

;; Fixit
;; a. This solution tries to use the remove to remove items from a list but this does not change the list, it simply returns it. It should be:
(defun summit (lst)
  (setf lst (remove nil lst))
  (apply #'+ lst))

;; b. This solution results in an infinite loop as if the exit condition (null x) is true, the function recursively inputs an empty list.
(defun summit (lst)
  (let ((x (car lst)))
    (cond ((and (null x)
                (null (cdr lst)))
           0)
          ((null x)
           (summit (cdr lst)))
          (t
           (+ x (summit (cdr lst)))))))
