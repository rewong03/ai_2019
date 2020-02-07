(defun coord-member (key sequence)
  (loop for item in sequence
        if (equalp key (car item))
        return t
        finally (return nil)))

(defun coord-assoc (key sequence)
  (loop for item in sequence
        if (equalp key (car item))
        return (cdr item)
        finally (return nil)))

(defun all-visited (current-cell visited l h)
  (let ((possible-cells (possible-coords (first current-cell)
                                         (second current-cell)
                                         l h)))
    (loop for cell in possible-cells
          if (not (member cell visited :test #'equalp))
          return nil
          finally (return t))))

(defun update-maze (current-cell new-cell maze)
  (if (coord-member current-cell maze)
      (let* ((old-cell (append (list current-cell)
                               (coord-assoc current-cell maze)))
             (updated-old-cell (append old-cell
                                       (list new-cell))))
        (append (list updated-old-cell)
                (remove old-cell maze :test #'equalp)))
      (append (list (list current-cell new-cell))
              maze)))

(defun possible-coords (x y l h)
  (cond ((and (= x 0)
              (= y 0))
         (list (list (+ x 1) y)
               (list x (+ y 1))))
        ((and (= x l)
              (= y 0))
         (list (list (- x 1) y)
               (list x (+ y 1))))
        ((and (= x 0)
              (= y h))
         (list (list (+ x 1) y)
               (list x (- y 1))))
        ((and (= x l)
              (= y h))
         (list (list (- x 1) y)
               (list x (- y 1))))
        ((= x 0)
         (list (list (+ x 1) y)
               (list x (+ y 1))
               (list x (- y 1))))
        ((= x l)
         (list (list (- x 1) y)
               (list x (+ y 1))
               (list x (- y 1))))
        ((= y 0)
         (list (list x (+ y 1))
               (list (- x 1) y)
               (list (+ x 1) y)))
        ((= y h)
         (list (list x (- y 1))
               (list (- x 1) y)
               (list (+ x 1) y)))
        (t
         (list (list (+ x 1) y)
               (list (- x 1) y)
               (list x (+ y 1))
               (list x (- y 1))))))

(defun backtrace-coord (stack visited-cells l h)
  (if (not (all-visited (first stack) visited-cells l h))
      (first stack)
      (backtrace-coord (rest stack)
                       visited-cells
                       l h)))

(defun maze-gen (current-cell l h
                 &key (visited-cells '())
                      (maze (list (list current-cell)))
                      (stack '()))
  (cond ((= (length visited-cells)
            (* (+ l 1)
               (+ h 1)))
         maze)
        ((member current-cell visited-cells :test #'equalp)
         (progn (print 'branch1)
                (maze-gen (first stack) l h
                          :visited-cells (rest visited-cells)
                          :maze (rest maze)
                          :stack (rest stack))))
        ((null (set-difference (possible-coords (first current-cell) (second current-cell) l h)
                               visited-cells
                               :test #'equalp))
         (let ((last-step (backtrace-coord stack visited-cells l h))) ; error seems to be somewhere in here
           (print 'branch2)
           (maze-gen last-step l h
                     :visited-cells (append (list current-cell)
                                            (remove last-step visited-cells :test #'equalp))
                     :maze maze
                     :stack (subseq stack (position last-step stack)))))
        (t
         (let* ((possibilities (set-difference (possible-coords (first current-cell) (second current-cell) l h)
                                               visited-cells
                                               :test #'equalp))
                (new-cell (elt possibilities
                               (random (length possibilities)))))
           (maze-gen new-cell
                     l h
                     :visited-cells (append (list current-cell)
                                            visited-cells)
                     :maze (update-maze current-cell new-cell maze)
                     :stack (append (list new-cell) stack))))))

