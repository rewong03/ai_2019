(defun is-var-p (symbol)
  (and (symbolp symbol)
       (equal #\?
              (elt (symbol-name symbol) 0))))

(defun match-p (sym1 sym2)
  (cond ((equal sym1 sym2)
         t)
        ((or (is-var-p sym1)
             (is-var-p sym2))
         t)
        (t
         nil)))

(defun segment-p (pat)
  (if (listp pat)
      (equal '?* (first pat))))

(defun segment-match-helper (pat input
                             &key (rest-input nil))
  (let ((idx (position (second pat) input)))
    (cond ((and (not idx)
                (null (second pat)))
           input)
          ((not idx)
           nil)
          ((pat-match (rest pat)
                      (subseq input idx))
           (append rest-input
                   (subseq input 0 idx)))
          (t
           (segment-match-helper pat (subseq input (+ idx 1))
                                 :rest-input (append rest-input
                                                     (subseq input 0 (+ idx 1))))))))

(defun segment-match (pat input)
  (let ((matched-pattern (segment-match-helper pat input)))
    (if matched-pattern
        (cons (second (first pat)) matched-pattern)
        nil)))

(defun match-var (pat stmt vars)
  (or (equal stmt
             (cdr (assoc pat vars)))
      (null (assoc pat vars))))

(defun pat-match (pat stmt
                  &key (vars nil))
  (cond ((and (null pat)
              (null stmt))
         (if vars
             vars
             (cons T T)))
        ((equal (first pat)
                (first stmt))
         (pat-match (rest pat)
                    (rest stmt)
                    :vars vars))
        ((segment-p (first pat))
         (let ((segment (segment-match pat stmt)))
           (if segment
               (if (match-var (first segment) 
                              (rest segment)
                              vars)
                   (if (assoc (first segment) vars)
                       (pat-match (rest pat)
                                  (subseq stmt (length (rest segment)))
                                  :vars vars)
                       (pat-match (rest pat)
                                  (subseq stmt (length (rest segment)))
                                  :vars (cons segment vars)))))))
        ((is-var-p (first pat))
         (if (match-var (first pat)
                        (first stmt)
                        vars)
             (if (assoc (first pat) vars)
                 (pat-match (rest pat) (rest stmt) :vars vars)
                 (pat-match (rest pat)
                            (rest stmt)
                            :vars (acons (first pat)
                                         (first stmt)
                                         vars)))
             nil))
        ((and (listp (first pat))
              (listp (first stmt)))
         (cons (pat-match (first pat)
                          (first stmt)
                          :vars vars)
               (pat-match (rest pat)
                          (rest stmt))))
        (t
         nil)))



