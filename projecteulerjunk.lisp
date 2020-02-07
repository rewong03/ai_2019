;;;; Welcome to Portacle, the Portable Common Lisp Environment.
;; For information on Portacle and how to use it, please read
;;   https://portacle.github.io or *portacle-help*
;; To report problems and to get help with issues,  please visit
;;   https://github.com/portacle/portacle/issues
;; Portacle is currently running SLIME , but you can
;;   Switch to SLY
;; You should also configure Portacle with the
;;   First-time setup
;; 
;; You can use this buffer for notes and tinkering with code.
(defparameter BIG-NUMBER-RAW
  (with-open-file (file "08-data.txt")
    (loop for line = (read-line file nil)
          while line
          collect line)))

(defparameter BIG-NUMBER-STRING
  (apply #'concatenate 'string big-number-raw))

(defparameter big-number
  (parse-integer big-number-string))

(defparameter big-number-list
  (loop for x from 0 to (- (length big-number-string) 1)
        collect (parse-integer (subseq big-number-string x (+ x 1)))))

(defun product (num-list product)
  (if (null num-list)
      product
      (product (rest num-list)
               (* product (first num-list)))))

(defparameter test
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))

(defun problem8 ()
  (loop for x from 0 to (- (length big-number-list)
                           13)
        maximize (product (subseq big-number-list x (+ x 13)))))


(Defparameter known-primes '(2 3))

(defun is-prime (num primes)
  (cond ((> (first primes)
             (sqrt num))
         t)
        ((= (mod num (first primes))
            0)
         nil)
        (t
         (is-prime num (rest primes)))))

(defun problem7 (num)
  (cond ((= (length known-primes)
            10001)
         (last known-primes))
        ((is-prime num known-primes)
         (progn (setf known-primes (append known-primes (list num)))
                (problem7 (+ num 1))))
        (t
         (problem7 (+ num 1)))))

(defun keepdivide (num factor)
  (if (= (mod num factor) 0)
      (keepdivide (/ num factor) factor)
      num))

(defun problem3 (num factor)
  (cond ((= factor num)
         num)
        ((= (mod num factor) 0)
         (problem3 (keepdivide num factor) (+ factor 1)))
        (t
         (problem3 num (+ factor 1)))))

(defun p3 (num)
  (let ((time (get-universal-time)))
    (problem3 num 2)
    (print (- (get-universal-time) time))))


(defun count-func-primes (func x)
  (if (is-prime (funcall func x (func)) known-primes)
      (+ 1 (count-func-primes func (+ x 1)))
      1))

(defun p27-helper (a primes most-primes idx)
  (cond ((> (first primes)
            10001)
         idx)
        ((< most-primes
            (count-func-primes (lambda (x) (+ (expt x 2)
                                              (* a x)
                                              (first primes)))
                               0))
         (p27-helper a (rest primes) (count-func-primes (lambda (x) (+ (expt x 2)
                                                                       (* a x)
                                                                       (first primes)))
                                                        0)
                     (first primes)))))

(defun p27 ()
  (loop for x from -1000 to 1000
        ))
