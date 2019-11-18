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
        ((or (is-var-p (first pat))
             (is-var-p (first stmt)))
         (pat-match (rest pat)
                    (rest stmt)
                    :vars (acons (first pat)
                                 (first stmt)
                                  vars)))
        ((and (listp (first pat))
              (listp (first stmt)))
         (cons (pat-match (first pat)
                          (first stmt)
                          :vars vars)
               (pat-match (rest pat)
                          (rest stmt))))
        (t
         nil)))

(defun segment-match (pat input)
  (let ((idx (position (second pat) input)))
    (if idx
        (subseq input 0 idx)
        idx)))


