(defun find-all (item sequence
                 &key (test #'eql) test-not)
  (cond ((null sequence)
         nil)
        (test-not
         (reverse (set-difference sequence
                                  (find-all item sequence :test test))))
        ((funcall test item (first sequence))
         (append (list (first sequence))
                 (find-all item (rest sequence)
                           :test test :test-not test-not)))
        (t
         (find-all item (rest sequence)
                   :test test :test-not test-not))))

(defun partition-if (pred list)
  (let ((satisfies-pred (remove-if-not pred list)))
    (list satisfies-pred
          (reverse (set-difference list
                                   satisfies-pred))))))
