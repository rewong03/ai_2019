; (addlength) (SC 3.12) Write a function that takes a list as input and returns a new list with the length of the input added onton the front of it.
(defun addlength (items)
  (append (list (length items))
          items))

; (crank-call) (SC 3.14) (a) Without running the function: what is the result of (crank-call 'wanda 'fred)?
; '(hello callee this is caller calling)
; (b) Modify the function so it behaves as a non-crank call using “quasiquote” but not “list”.
(defun crank-call (caller callee)
  `(hello ,callee this is ,caller calling))

; (SC Section 4.8, EX 4.14) Carefully state the rules for evaluating and and or. What is (and 'a 'b)? What is (or nil 'bank 'money)? Give the results of each without typing them:
; a. T
; b. T
; c. T
; d. Nil
; e. T.
; f. T. 

; (same-sign) (SC, Page 124) Write a function (same-sign a b) that returns true if a and b have the same sign (choices: +,-, or 0).
(defun same-sign (a b)
  (or (equal a b)
      (and (> a 0)
           (> b 0))
      (and (< a 0)
           (< b 0))))

; Exercise 6.41

(defun choices (location)
  (loop for room in rooms
        if (equal location (first room))
        return (rest room)))

(defun look (location direction)
  (loop for option in (choices location)
        if (equal direction (first option))
        return (first (rest option))))

(defvar LOC 'pantry)
(defun change-loc (new-location)
  (setf LOC new-location))

(defun how-many-choices ()
  (length (choices LOC)))

(defun upstairsp (location)
  (or (equal location 'upstairs-bedroom)
      (equal location 'library)))
(defun onstairsp (location)
  (or (equal location 'front-stairs)
      (equal location 'back-stairs)))

(defun where ()
  (cond ((upstairsp LOC)
         `(Robbie is upstairs in the ,LOC))
        ((onstairsp LOC)
         `(Robbie is on the ,LOC))
        (t
         `(Robbie is downstairs in the ,LOC))))

(defun move (direction)
  (let ((new-dir (look LOC direction)))
    (if new-dir
        (progn (change-loc new-dir)
               (where))
        '(Oof! Robbie hit a wall))))


(defun go-through-house ()
  (setf LOC 'pantry)
  (move 'west)
  (move 'west)
  (move 'north)
  (move 'north)
  (move 'east)
  (move 'south)
  (move 'south)
  (move 'east))
