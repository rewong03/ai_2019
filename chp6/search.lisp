(defun binary-tree (state
                    &key (max 16))
  "Finds next nodes in a binary tree for the first  
   node in the inputted list of nodes"
  (let ((node (first state)))
    (if (< max
           (+ (* 2 node) 1))
        nil
        (list (* 2 node)
              (+ (* 2 node) 1)))))

(defun depth-first-search (state goal
                           &key (successor #'binary-tree)
                                (optimizer (lambda (paths) (first paths))))
  "Depth-first search algorithm. Takes in a list with the starting point,
   the goal, and a successor function. Returns the first path found."
  (let ((current-node (first state)))
    (cond ((equal current-node goal)
           state)
          ((null current-node)
           nil)
          (t
           (loop for branch in (funcall successor state)
                 for path = (depth-first-search (append (list branch) state)
                                                 goal
                                                 :successor successor
                                                 :optimizer optimizer)
                 if path
                 collect path into good-paths
                 finally (return (funcall optimizer good-paths)))))))

(defun breadth-first-search-helper (state goal
                                    &key (successor #'binary-tree)
                                         (good-paths '()))
  "Breadth-first search algorithm. Takes in a list with the starting point,
   the goal, and a successor function. Returns the first path it finds."
  (let* ((path (first state))
         (node (first path)))
    (print path)
    (cond ((equalp goal node)
           (progn (print 'here)
                  (append path good-paths)))
          ((null state)
           (min-dist good-paths))
          ((null (funcall successor path))
           (breadth-first-search-helper (rest state) goal
                                        :successor successor
                                        :good-paths good-paths))
          (t
           (let ((new-paths
                   (loop for item in (funcall successor path)
                         collect (append (list item) path))))
             (breadth-first-search-helper (append (rest state) new-paths) goal
                                          :successor successor
                                          :good-paths good-paths))))))

(defun breadth-first-search (state goal
                             &key (successor #'binary-tree)
                                  (good-paths '()))
  (breadth-first-search-helper (list state) goal :successor successor :good-paths good-paths))

(defstruct (city (:type list)) name long lat)
(defparameter *cities*
  '((Atlanta 84.23 33.45)
    (Los-Angeles 118.15 34.03)
    (Boston 71.05 42.21)
    (Memphis 90.03 35.09)
    (Chicago 87.37 41.50)
    (New-York 73.58 40.47)
    (Denver 105.00 39.45)
    (Oklahoma-City 97.28 35.26)
    (Eugene 123.05 44.03)
    (Pittsburgh 79.57 40.27)
    (Flagstaff 111.41 35.13)
    (Quebec 71.11 46.49)
    (Grand-Jet	108.37 39.05)
    (Reno 119.49 39.30)
    (Houston 105.00 34.00)
    (San-Francisco 122.26 37.47)
    (Indianapolis 86.10 39.46)
    (Tampa 82.27 27.57)
    (Jacksonville 81.40 30.22)
    (Victoria 123.21 48.25)
    (Kansas-City 94.35 39.06)
    (Wilmington 77.57 34.14)))

(defun torad (deg)
  (* pi (/ deg 180)))

(defun coord-dist (city1 city2)
  "Calculates distance between two city structs."
  (let* ((lat1 (city-lat city1))
         (long1 (city-long city1))
         (lat2 (city-lat city2))
         (long2 (city-long city2))
         (dlat (torad (- lat2 lat1)))
         (dlong (torad (- long2 long1)))
         (earth-rad 6371)
         (a (+ (expt (sin (/ dlat 2)) 2)
               (* (cos (torad lat1))
                  (cos (torad lat2))
                  (expt (sin (/ dlong 2)) 2))))
         (c (* 2
               (atan (sqrt a)
                     (sqrt (- 1 a))))))
    (* earth-rad c)))

(defun get-city (city-name)
  (assoc city-name *cities*))

(defun neighbor (state
                 &key (max-dist 1000))
  "Finds cities close to first item of state that aren't
   in the rest of state. Takes in a list of cities and 
   returns a list of cities."
  (let ((city (first state)))
    (loop for item in *cities*
          if (and (not (member item state))
                  (< (coord-dist city item) max-dist))
          collect item)))

(defun total-dist (path)
  (if (null (rest path))
      0
      (+ (+ (coord-dist (first path)
                        (second path)))
         (total-dist (rest path)))))

(defun min-dist (paths)
  "Given a list of paths between cities, returns the path
   with the shortest total distance."
  (let ((min (loop for path in paths minimize (total-dist path))))
    (car (rassoc min
                 (loop for path in paths
                       collect `(,path . ,(total-dist path)))))))






