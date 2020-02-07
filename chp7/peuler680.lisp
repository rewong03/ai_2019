                                        ; Project Euler 680

(defun fibmodhelp (n m &key (a 0)
                        (b 1))
  (if (= n 0)
      '()
      (cons (mod (+ a b) m)
            (fibmodhelp (- n 1) m
                    :b a
                    :a (mod (+ a b)
                            m)))))

(defun fibmod (n m)
  (cons 0 (fibmodhelp n m)))

(defun flip (as s end)
  (append (subseq as 0 s)
          (reverse (subseq as s end))
          (subseq as end)))

  
