(defstruct op name prereq gain lose)

(defun achieve-one (state goal
                    &key (remaining-goals nil)
                         (goal-stack nil)
                         (ops *test-big-ops*)
                         (achieve-all #'achieve-all))
  )

(defun achieve-all (state goals
                    &key (achieve-one #'achieve-one)
                         (goal-stack nil)
                         (ops *test-big-ops*))
  (cond ((all-achieved goals state)
         state)
        ((funcall achieve-one state (first goals)
                  :remaining-goals (rest goals)
                  :achieve-all #'achieve-all)
         (achieve-all (setf state (funcall achieve-one state (first goals)
                                           :remaining-goals (rest goals)
                                           :achieve-all #'achieve-all))
                      (rest goals)
                      :achieve-one achieve-one
                      :goal-stack goal-stack
                             
