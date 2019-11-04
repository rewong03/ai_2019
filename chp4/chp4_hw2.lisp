(defstruct op name (prereq nil) (gain nil) (lose nil))

(defparameter *test-big-ops*
  *school-ops*)


(defparameter *make-cake*
  (list
   (make-op :name 'bake-cake
            :prereq '(flour sugar)
            :gain '(cake)
            :lose '(flour sugar))
   (make-op :name 'buy-flour
            :prereq '(have-money)
            :gain '(flour)
            :lose '(have-money))
   (make-op :name 'borrow-sugar
            :prereq '(nice-neighbor)
            :gain '(sugar))))

(defparameter *school-ops*
  (list
   (make-op :name 'drive-son-to-school
            :prereq '(son-at-home car-works)
            :gain '(son-at-school)
            :lose '(son-at-home))
   (make-op :name 'shop-installs-battery
            :prereq '(car-needs-battery shop-knows-problem shop-has-money)
            :gain '(car-works))
   (make-op :name 'tell-shop-problem
            :prereq '(in-communication-with-shop)
            :gain '(shop-knows-problem))
   (make-op :name 'telephone-shop
            :prereq '(know-phone-number)
            :gain '(in-communication-with-shop))
   (make-op :name 'look-up-number
            :prereq '(have-phone-book)
            :gain '(know-phone-number))
   (make-op :name 'give-shop-money
            :prereq '(have-money)
            :gain '(shop-has-money)
            :lose '(have-money))))


(defun achieve-one (state goal
                    &key (remaining-goals nil)
                         (goal-stack nil)
                         (ops *test-big-ops*)
                         (achieve-all #'achieve-all))
  (cond ((all-achieved (list goal) state)
         state)
        (t
         (loop for operation in (find-appropriate-1 ops goal)
               for temp = (funcall achieve-all state (op-prereq operation))
               when (all-achieved (op-prereq operation)
                                temp)
               do (print (op-name operation))
               and return (apply-op temp operation)
               else return state))))



(defun achieve-all (state goals
                    &key (achieve-one #'achieve-one)
                         (goal-stack nil)
                         (ops *test-big-ops*))
  (cond ((all-achieved goals state)
         state)
        (t
         (loop for goal in goals
               do (setf state (funcall achieve-one state goal))
               finally (return state)))))

