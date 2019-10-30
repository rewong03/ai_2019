(defstruct op name prereq gain lose)

(defun list1 (lst)
  "Takes a list and returns the last item of the list"
  (if (null (rest lst))
      (first lst)
      (list1 (rest lst))))

(defun single (lst)
  "Takes a list and returns whether the list only contains one element"
  (and lst
       (null (rest lst))))

(defun append1 (lst item)
  "Takes an item and a list and appends the item to the end of the list"
  (append lst (list item)))

(defun mklist (obj)
  "Takes an item and puts it in a list if it isn't in one"
  (if (listp obj)
      obj
      (list obj)))

(defun longer (x y)
  "Give true if the list x is longer than y"
  (cond ((null y)
         t)
        ((null x)
         nil)
        (t
         (longer (rest x)
                 (rest y)))))

(defun remove-* (subtract all)
  "Return items from all that aren't in subtract"
  (reverse (set-difference all subtract)))

(defun member-* (x xs)
  (loop for item in xs
        when (equalp x item) return t))

(defun all-achieved (wanted got)
  "Returns got if got is a subset of wanted"
  (cond ((null wanted)
         t)
        ((member-* (first wanted)
                   got)
         (all-achieved (rest wanted)
                       got))
        (t
         nil)))

(defun apply-op (state op1)
  "Adds gain list from op1 to state and removes items from state"
  (append (remove-* (op-lose op1) state) (op-gain op1)))

(defun appropriate-p (op1 goal-list)
  "Returns true if op1 produces a goal in goal-list"
  (loop for item-gain in (op-gain op1)
        when (member-* item-gain goal-list) return t))

(defun find-appropriate-1 (ops goal)
  "Returns every op in ops that immediately gains goal"
  (loop for operation in ops
        when (member-* goal (op-gain operation)) collect operation))
