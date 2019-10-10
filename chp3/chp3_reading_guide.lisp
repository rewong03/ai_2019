;; Chp3 Reading Guide

;; Questions
;; 1. The three most important maxims to me are consistency, being concise, and being specific. An example of being consistent is using
;;    the same function and naming scheme is the same all the time. An example of being concise is doing (last some-list) vs. (rest (rest (rest .. (rest some-list)))).
;;    They both do the same thing but doing (last some-list) is more concise. An example of being specific is using when vs. if when you only have one condition as when
;;    is more specific to this case.
;; 2. caddar takes the third item from the first item in a list.
;; 3. Find is preferred because it is more concise and more abstract.
;; 4. You can use (setq foo bar) or (setf foo bar). They both do the same thing but setq is considered more specific while setf is considered more consistent. However
;;    regardless of which ever one you use you should always use that same function.

;; Special Forms
;; 1. defmacro, labels
;; 2. incf takes two numbers and increments the first number by the second while decf decrements the first number by the second.
;; 3. (defvar x 5) will set x to 5 if x has no value.

;; Student
;; 1. Write the form to define a student structure that contains name, division, id, and an optional home-phone which is set to nil if it is not available.
(defstruct student
  name
  division
  id
  (home-phone nil))
;; 2. Create a student with name DocMo, division A321, and id number 666321.
(defparameter docmo (make-student :name 'DocMo
                                  :division 'A321
                                  :id 666321))
;; 3. Create a student with your name, division, id number, and a home phone of 773-534-7500.
(defparameter ryan (make-student :name 'Ryan
                                 :division 'A141
                                 :id 123666
                                 :home-phone '773-534-7500))
;; 4. To check whether v is a student you can just do (student-p v)
;; 5. Given the student from two exercises ago, how do you make a list containing just the name and division number.
(defparameter ryan-name-div (list (student-name ryan)
                                  (student-division ryan)))
;; 6. Set the division of DocMo to 934.
(setf (student-division docmo) 'A934)
;; 7. It is equally efficient to create the list/structure but to access items it is more efficient to use a structure.

;; Conditionals
;; 1. The most basic special form conditional is if.
;; 2. (if good-friend nil (run-fast)) -> (unless good-friend (run-fast))
;; 3. IDK.
;; 4. (prin "some string")
;; 5. Read the examples using and or or to run conditionals and then write an example that prints out “Go Fish” if the
;;    function q19 is called with neither the color 'red nor the suit 'clubs.
(defun q19 (color suit)
  (and (not (eq color 'Red))
       (not (eq suit 'Clubs))))
;; 6. cond and if are preferred when returning values and when and unless are good for taking action.
;; 7. The first true test is evaluated.
(defun all-true (num)
  "Each branch can evaluate to true if 1 is input"
  (cond ((eq num 1)
         1)
        ((> 0 num)
         2)
        ((< 3 num)
         3)
        (t 4)))
;; 8. typecase
;; 9. Write a function that doubles any number it is given and returns only the first element if given a list.
(defun sometimes-double (x)
  (typecase x
    (number (* x 2))
    (list (first x))
    (t "IDK")))

;; Repetition
;; 1. mapc takes in a function and a list and applies the function to the elements but returns the inputted list rather
;;    than the results of applying the function to the list.
;; 2. delete takes in a value and a list and omits the values in the list that match the value. remove is similar except
;;    it makes a copy of the list before ommiting values. There also is a remove-if and a remove-if-not that takes a predicate.
;; 3. You can either use position or position-if.
;; 4. length10 is a tail recursive function, meaning it completes all other operations before recursively calling itself, freeing up
;;    memory to be used, but length9 is not tail recursive, meaning that when it recursively calls itself, there are remaining operations
;;    that require memory to be retained.

;; Other
;; 1. The value of (progn 1 2 3) is 3.
;; 2. progn is useful when evaluating a sequence of forms.
;; 3. Doing (magic (list 5 10 40 8)) is equal to 240000. None of the values is 42 so each value gets multiplied to ans which is then multiplied to 3.

;; Macros
;; 1. Macros are essentially "forms" of code that are shorthands for larger amounts of code.
;; 2. macroexpand-l

;; Equality and sequences
;; 1. = tests whether two numbers are the same, eq tests for the exact same object, eql is a combination of eq and =, equal tests for eql and lists, or strings,
;;    equalp is like equal but ignores cases of letters and types of numbers.
;; 2. nth is different form the other "getter" functions because it is the only one that takes the indice as the first value.
;; 3. assoc searches a list for key but rassoc searches a list for value.
;; 4. Association tables search for items one at a time which can be slow.
;; 5. Hash tables can be used to search for values faster, but creating hash tables has a considerable overhang, so it is only useful for large tables.

;; Excercises
;; 1. Do something different depending on the type of an input. Error if not one of the choices.
(defun e1 (some-input)
  (etypecase some-input
    (number 'Number!)
    (list 'List!)))

;; 2. Do something different depending on the type of an input. Error if not one of the choices.
(defun e2 (some-list)
  (loop for x from 0 to 2
        collect (nth x some-list)))

;; 3. Write a lambda expression equivalent to:
;; (let* ((x 6)
;;        (y (* x x)))
;;   (+ x y))
(defun e3 (x)
  (funcall #'(lambda (x) (+ x
                            (funcall #'(lambda (x) (* x x)) x)))
           x))
