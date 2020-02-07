;; Project Euler 54

(defun value-to-num (value)
  (let ((royals '(T J Q K A)))
    (cond ((member value royals)
           (+ 10 (position value royals)))
          (t
           value))))

(defun get-val (card)
  (read-from-string (subseq (symbol-name card)
                            0 1)))
(defun get-suit (card)
  (read-from-string (subseq (symbol-name card)
                            1 2)))

(defun same-suit (hand)
  (let* ((suits (mapcar #'get-suit hand))
         (suit (first suits)))
    (every (lambda (x) (eql x suit))
           suits)))

(defun royal-flush (hand)
  (and (subsetp (mapcar (lambda (x) (value-to-num (get-val x))) hand)
                '(10 11 12 13 14))
       (same-suit hand)))

(defun is-ascending (nums)
  (cond ((null (second nums))
         t)
        ((= (+ 1 (first nums))
            (second nums))
         (is-ascending (rest nums)))
        (t
         nil)))

(defun straight-flush (hand)
  (let ((values (mapcar (lambda (x) (value-to-num (get-val x))) hand)))
    (if (and (is-ascending (sort values #'<))
             (same-suit hand))
        (apply #'max values))))

(defun n-kind (hand n)
  (let ((values (mapcar (lambda (x) (value-to-num (get-val x))) hand)))
    (loop for value in values
          if (eql n (count value values))
          do (return value))))

(defun four-of-a-kind (hand)
  (n-kind hand 4))

(defun three-of-a-kind (hand)
  (n-kind hand 3))

(defun one-pair (hand)
  (n-kind hand 2))

(defun two-pair (hand)
  )

(defun full-house (hand)
  (and (three-of-a-kind hand)
       (one-pair hand)))



(defun moobuzz (num)
  (let ((solutions '(1 2 4 7 8 11 13 14))
        (q (floor num 8))
        (r (mod num 8)))
    (+ (* 15 q)
       (nth (- r 1) solutions))))
