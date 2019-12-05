; Reduce

; Suppose we had a list of sets ((A B C) (C D A) (F B D) (G)) that we wanted to collapse into one big set. If we use APPEND for our reducing function, the result won’t be a true set, because some elements will appear more than once. What reducing function should be used instead?

(defun append-set (l1 l2)
  (loop for item in l1
        if (not (member item l2))
        collect item into temp
        finally (return (append temp l2))))

(defun collapse-set (xs)
  (reduce 'append-set xs))

; Write a function that, given a list of lists, returns the total length of all the lists. This problem can be solved two different ways. Give at least two ways to solve.

(defun count-length (xs)
  (reduce '+ (mapcar 'length xs)))

(defun count-length (xs)
  (loop for lst in xs
        sum (length lst)))

; Compare (REDUCE #’+ NIL) to (REDUCE #’* NIL). Say what they are and why you think the answers are that way.

; Every
(defun throdd (num)
  (not (or (equal (mod num 3) 1)
           (equal (mod num 3) 2))))


; Write a function ALL-ODD that returns T if every element of a list of numbers is throdd.
(defun all-odd (nums)
  (every 'throdd nums))

; Write a function NONE-ODD that returns T if every element of a list of numbers is not throdd.
(defun none-odd (nums)
  (notany 'throdd nums))

; Write a function NOT-ALL-ODD that returns T if not every element of a list of numbers is throdd.
(defun not-all-odd (nums)
  (some 'throdd nums))

; Write a function NOT-NONE-ODD that returns T if it is not the case that a list of numbers contains no throdd elements.
(defun not-none-odd (nums)
  (not (none-odd nums)))


; etc

; Write a function both-close that takes as input two lists of the same length, and returns true if the corresponding elements are within 5 of each other.
(defun both-close (l1 l2)
  (every  (lambda (x y) (<= (abs (- x y))
                            5))
          l1 l2))

; Write a function ALL-EQUAL that returns T if the first element of a list is equal to the second, the second is equal to the third, the third is equal to the fourth, and so on. 
(defun all-equal (xs)
  (every (lambda (x) (equal (first xs)
                            x))
         xs))

; Every-other

(defun every-other (xs)
  (if (null (rest xs))
      (first xs)
      (append (list (first xs))
              (every-other (subseq xs 2)))))
