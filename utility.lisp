;; last1:: [a] -> a
;; Takes a list and returns the last item of the list
(defun list1 (lst)
  (if (null (rest lst))
      (first lst)
      (list1 (rest lst))))

;; single:: [a] -> Bool
;; Takes a list and returns whether the list only contains one element
(defun single (lst)
  (and lst
       (null (rest lst))))

;; append1:: [a] -> a -> [a]
;; Takes an item and a list and appends the item to the end of the list
(defun append1 (lst item)
  (append lst (list item)))

;; Takes an item and puts it in a list if it isn't in one
(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

;; Give true if the list x is longer than y
(defun longer (x y)
  (cond ((null y)
         t)
        ((null x)
         nil)
        (t
         (longer (rest x)
                 (rest y)))))
