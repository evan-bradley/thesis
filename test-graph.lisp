(defvar *test-graph* (make-hash-table))

(defvar *init-geom*
  (%make-win-geometry
   ;; :id 16777221
   :id 0
   :center (make-point :x 479 :y 588)
   :min (make-point :x 0 :y 0)
   :max (make-point :x 958 :y 1176)))

(defvar *init-node*
  (make-win-node
   :win nil
   :id (win-geometry-id *init-geom*)
   :geometry *init-geom*
   :up (make-edge :source-node (win-geometry-id *init-geom*) :target-node nil :dir 'up :dist 'inf)
   :down (make-edge :source-node (win-geometry-id *init-geom*) :target-node nil :dir 'down :dist 'inf)
   :left (make-edge :source-node (win-geometry-id *init-geom*) :target-node nil :dir 'left :dist 'inf)
   :right (make-edge :source-node (win-geometry-id *init-geom*) :target-node nil :dir 'right :dist 'inf)))

(setf (gethash (win-node-id *init-node*) *test-graph*) *init-node*)

(defun print-graph (graph)
  (loop
    for key being the hash-keys in graph using (hash-value node)
    do (print node)))

(defun gt (id)
  "geom-test: quickly make new geom object"
  (%make-win-geometry
   :id id
   :center (make-point :x 479 :y 588)
   :min (make-point :x 0 :y 0)
   :max (make-point :x 958 :y 1176)))

(defun quick-add ()
  (let* ((num (hash-table-count *test-graph*)))
    (setf (gethash num *test-graph*) (new-node-in-graph (gt num) 0 *test-graph*))
    (update-all-edges *test-graph*)))
