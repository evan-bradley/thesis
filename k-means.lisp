(defun sq-euclid-distance (p q)
  (cond ((or (null p) (null q)) 0)
        (t (+ (expt (- (car p) (car q)) 2)
              (sq-euclid-distance (cdr p) (cdr q))))))

(defun euclid-distance (p q) (sqrt (sq-euclid-distance p q)))

(defun add-points (p)
  (list (+ (first (first p)) (first (second p)))
        (+ (second (first p)) (second (second p)))))

(defun div-point (p divisor)
  (list (/ (first p) divisor) (/ (second p) divisor)))

;; From https://gist.github.com/nowl/918623
(defun classify (means data dist-func)
  (let ((sets (loop for m in means collect '())))
    (loop for d in data do
         (let ((min 0)
               (dist (funcall dist-func d (car means))))
           (loop for m in (cdr means) for n from 1 do
                (when (< (funcall dist-func d m) dist)
                  (setf min n
                        dist (funcall dist-func d m))))
           (push d (nth min sets))))
    sets))

(defun update-means (sets sum-func div-func)
  (loop for set in sets collect
       (funcall div-func
                (funcall sum-func set)
                (length set))))

(defun k-means (k data sum-func div-func dist-func)
  ;; randomly assign the data into k sets
  (let ((sets (loop with d = (copy-list data) for i below k collect
                   (loop for j below (/ (length data) k) while (plusp (length d)) collect
                        (let ((new (random (length d))))
                          (prog1
                              (nth new d)
                            (setf d (delete (nth new d) d))))))))
    (loop with converged = nil for i below 100 while (not converged) do
         (let ((prev-sets (copy-list sets)))
           ;; classify the data
           (setf sets (classify (update-means sets sum-func div-func) data dist-func))
           (when (equalp sets prev-sets) (setf converged t))))
    sets))
