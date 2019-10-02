;;;; Chapter 1 Reading Guide

;; Set 1A
;; 1. SBCL stands for steel bake common lisp.
;; 2.
;; 3. Copy-paste
;; 4. C-x h, C-c
;; Set 1B
;; 5. Use Common Lisp to compute 10+30+50+80+200*30+84*97 = 14138
(+ 10 30 50 80 (* 200 30) (* 84 97))
;; 6. Create a list containing the following symbols: apple orange banana peach starfruit
(defparameter num6 `(apple orange banana peach starfruit))
;; 7. Create a list containing the following strings: "Alpha" "Bravo" "Charlie" "Delta"
(defparameter num7 `("Alpha" "Bravo" "Charlie" "Delta"))
;; 8. Strings have quotes while symbols don't have quotes.
;; 9. List processing.
;; 10. Put lists from #6 and #7 together in that order into one long list.
(defparameter num10 (append num6 num7))
;; 11. (length list)
;; 12. Yes $-per-peso is a legal name.
;; 13. You get an undefined variable error.
;; 14. (defparameter variable value)
;; 15. Special forms are syntactic expressions that serve as markers.
;; 16. setf is a special function while (setf x 3) is a special form.
;; 17. #' Is a symbol that maps a function name to the actual function.
;; 18. ` Is a symbol that prevents an expression from being evaluated.
;; 19. Given the list trial containing (list "a" "berry" "is" "sweet"), what is:
;; a. (length trial) = 4
;; b. (first trial) = "a"
;; c. (last trial) = "sweet"

;; Set 1C
;; 20. Write a function that takes in a list of names like '(Raymond James) (Sylvia Ellen Reshak) (John Q Public)) and returns all of them who have first name 'John (assume no honorifics are used).
(defun is-john (name)
  (if (eq `John (first name))
      name
      ()))
(defun return-john (names)
  (apply #'append (mapcar #'is-john names)))
;; 21. Write a function that takes in an integer and produces either half of that integer (if the input is even) or one more than three times the input (if the input is odd). Call your function COLLATZ-1. 
(defun collatz-1 (num)
  (if (= (rem num 2) 0)
      (/ num 2)
      (* num 3)))
;; 22. Write a function that takes in an integer and produces either half of that integer (if the input is even) or one more than three times the input (if the input is odd). Call your function COLLATZ-1. 
(defun sum-list (list)
  (apply #'+ list))
(defun summer (list)
  (mapcar #'sum-list list))
;; 23. In apply, arguments are passed in a list but in funcall the arguments are listed separately.
;; 24. Mapcar returns the number of items in the inputted list but mappend may not.
;; 25. A caret used to be placed over bound variables but the caret was then moved in front of the bound variable, which eventually morphed to a lambda.
;; 26. A simple benifit of lambda is that it can reduce the number of unnecessary functions and variables you use. A complex benifit is that it can create new functions at run time.
