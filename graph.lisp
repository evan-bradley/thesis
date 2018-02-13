;; Copyright (C) 2017-2018 Evan Bradley
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; This file handles all desktop graph functionality.
;;
;; Code:
(in-package :stumpwm)

(defstruct point x y)

(defstruct (win-geometry (:constructor %make-win-geometry))
  id
  center
  min
  max)

(defstruct edge
  source-node
  target-node
  dir
  dist)

(defstruct win-node
  win
  id
  geometry
  up
  down
  left
  right)

(defvar *last-focused* nil)
(defvar *window-graph* (make-hash-table))
(defvar *clusters-hash* (make-hash-table))
(defvar *win-to-cluster* (make-hash-table))

(defun get-list-bounding-box (list-of-windows &key min-x min-y max-x max-y )
  "Get the coordinates that contain a given list of windows."
  (if (null list-of-windows)
      (list (list min-x min-y) (list max-x max-y))
      (let* ((win (car list-of-windows))
             (parent (window-parent win))
             (win-min-x (xlib:drawable-x parent))
             (win-min-y (xlib:drawable-y parent))
             (win-max-x (+ win-min-x (xlib:drawable-width parent)))
             (win-max-y (+ win-min-y (xlib:drawable-height parent))))
        (cond ((or (null min-x) (null min-y) (null max-x) (null max-y))
               (setq min-x win-min-x)
               (setq min-y win-min-y)
               (setq max-x win-max-x)
               (setq max-y win-max-y)))
        (when (<= win-min-x min-x) (setq min-x win-min-x))
        (when (<= win-min-y min-y) (setq min-y win-min-y))
        (when (>= win-max-x max-x) (setq max-x win-max-x))
        (when (>= win-max-y max-y) (setq max-y win-max-y))
        (get-list-bounding-box
         (cdr list-of-windows) :min-x min-x :min-y min-y :max-x max-x :max-y max-y))))

(defun get-win-extents (win)
  "Get the extents for a single window"
  (get-list-bounding-box (list win)))

(defun make-win-geometry (win)
  (let* ((extents (get-win-extents win))
         (min-coords (first extents))
         (max-coords (second extents))
         (min-point (make-point
                     :x (first min-coords)
                     :y (second min-coords)))
         (max-point (make-point
                     :x (first max-coords)
                     :y (second max-coords)))
         (center-point (make-point
                        :x (/ (- (first max-coords) (first min-coords)) 2)
                        :y (/ (- (second max-coords) (second min-coords)) 2))))
    (%make-win-geometry
     :id (xlib:window-id (window-xwin win))
     :center center-point
     :min min-point
     :max max-point)))

(defun point->list (p)
  (list (point-x p) (point-y p)))

(defun graph->geom-list (graph)
  (loop
    for key being the hash-keys in graph using (hash-value node)
    collect (win-node-geometry node) into lst
    finally (return lst)))

(defun add-to-focus-history (win last-win)
  (setq *last-focused* (or last-win win)))

(add-hook *focus-window-hook* 'add-to-focus-history)

(defun find-window-to-side (cur-coord get-coord compare closest-win current-best lst)
  (if (null lst)
      (if (funcall compare (funcall get-coord closest-win) cur-coord)
          (values closest-win current-best)
          nil)
      (let ((cur-dist (abs (- cur-coord (funcall get-coord (car lst))))))
        (if (or (equal current-best nil) (< cur-dist current-best))
            (progn
              (setq current-best cur-dist)
              (setq closest-win (car lst))))
        (find-window-to-side cur-coord get-coord compare closest-win current-best (cdr lst)))))

(defun filter-outside-boundaries (min max fn lst)
  "Takes a max and min for the current window, and filters out
   all windows whose min coord is greater than the max, or whose
   max coord is less than the min."
  (remove-if (lambda (win)
               (or (< (funcall fn (win-geometry-max win)) min)
                   (> (funcall fn (win-geometry-min win)) max)))
             lst))

;; Only returns windows that cross the line on a particular axis.
(defun filter-outside-line (line fn lst)
  (remove-if-not (lambda (win)
                   (and (> line (funcall fn (win-geometry-min win)))
                        (< line (funcall fn (win-geometry-max win)))))
                 lst))

;; Take the direction and find the window with the nearest
;; (x, y) coordinates in that direction.
;; Move to that window (centered or on left).
;; Must consider both position and distance when finding window.
;;
;; +---A--+
;; |      |  +--B-+
;; |      |  +----+
;; +------+ +--C-+
;;          +----+
;; This would select B, not C.
;; ---------------------------
;;
;; +---A--+
;; |      |           +--B-+
;; |      |           +----+
;; +------+ +--C-+
;;          +----+
;; Would this select B or C?
;; ---------------------------
;;
;; +---A--+
;; |      |
;; |      |
;; +------+
;;
;;          +--B-+
;;          +----+
;; Probably would select B.
;; ---------------------------
;;
;; Cases:
;;   - Window is on side and intersects the middle-line of the current window.
;;   - Window is within the width/height of current window.
;;     - e.g. searching on right: window's top/bottom y-coords intersect the
;;       current window's top/bottom y-coords.
;;   - Window doesn't intersect either line, but is closest on corresponding side.
;;
;; Performs three levels of searching, stopping at a specified level:
;;       1.) Crosses middle
;;       2.) Within window borders
;;       3.) Anywhere on that side of the window.
(defun locate-closest-window (geom win-list dir &key level)
  (when (not (null win-list))
    (let* ((min-x (point-x (win-geometry-min geom)))
           (min-y (point-y (win-geometry-min geom)))
           (max-x (point-x (win-geometry-max geom)))
           (max-y (point-y (win-geometry-max geom)))
           (compare nil)
           (get-coord nil)
           (val 0)
           (x-median (point-x (win-geometry-center geom)))
           (y-median (point-y (win-geometry-center geom)))
           (dist 0)
           ;; (win-list-outside-boundaries nil)
           (closest-win nil)
           (win-list-bounds-filtered nil)
           (win-list-median-filtered nil))
      ;; TODO: Should use structure.
      (switch (dir :test #'equal)
        ("up"
         (setq compare #'<=)
         (setq get-coord (lambda (comp-geom) (point-y (win-geometry-max comp-geom))))
         (setq win-list-bounds-filtered (filter-outside-boundaries
                                         (point-x (win-geometry-min geom))
                                         (point-x (win-geometry-max geom))
                                         #'point-x win-list))
         (setq win-list-median-filtered (filter-outside-line x-median #'point-x win-list))
         (setq val min-y))
        ("down"
         (setq compare #'>=)
         (setq get-coord (lambda (comp-geom) (point-y (win-geometry-min comp-geom))))
         (setq win-list-bounds-filtered (filter-outside-boundaries
                                         (point-x (win-geometry-min geom))
                                         (point-x (win-geometry-max geom))
                                         #'point-x win-list))
         (setq win-list-median-filtered (filter-outside-line x-median #'point-x win-list))
         (setq val max-y))
        ("left"
         (setq compare #'<=)
         (setq get-coord (lambda (comp-geom) (point-x (win-geometry-max comp-geom))))
         (setq win-list-bounds-filtered (filter-outside-boundaries
                                         (point-y (win-geometry-min geom))
                                         (point-y (win-geometry-max geom))
                                         #'point-y win-list))
         (setq win-list-median-filtered (filter-outside-line y-median #'point-y win-list))
         (setq val min-x))
        ("right"
         (setq compare #'>=)
         (setq get-coord (lambda (comp-geom) (point-x (win-geometry-min comp-geom))))
         (setq win-list-bounds-filtered (filter-outside-boundaries
                                         (point-y (win-geometry-min geom))
                                         (point-y (win-geometry-max geom))
                                         #'point-y win-list))
         (setq win-list-median-filtered (filter-outside-line y-median #'point-y win-list))
         (setq val max-x)))
      ;;(print win-list-filtered)
      ;;(echo win-list-filtered)
      ;; (dformat 0 "Win-list: ~a~% Win-list-filtered: ~a~%" win-list win-list-filtered)
      ;; FIXME: There has to be a better way to do this than cascading if-statements.
      ;;        Consider looping through lists of available search functions.
      (multiple-value-setq
          (closest-win dist)
        (if (not (null win-list-median-filtered))
            (find-window-to-side val get-coord compare nil nil win-list-median-filtered)
            (values nil nil)))
      ;; First heuristic: search within windows touching a line running
      ;; through the middle of the window.
      (if (not (null closest-win))
          (values (win-geometry-id closest-win) dist)
          (progn
            ;; Perform a wider search that searches windows within the window's bounds
            ;; on the opposite axis.
            (when (not (eq level 'median))
              (multiple-value-setq
                  (closest-win dist)
                (if (not (null win-list-bounds-filtered))
                    (find-window-to-side val get-coord compare nil nil win-list-bounds-filtered)
                    (values nil nil))))
            (if (not (null closest-win))
                (values (win-geometry-id closest-win) dist)
                (progn
                  ;; Perform an even wider search that simply finds the closest window
                  ;; on that axis.
                  (when (not (or (eq level 'median) (eq level 'bounds)))
                    (multiple-value-setq
                        (closest-win dist)
                      (find-window-to-side val get-coord compare nil nil win-list)))
                  (if (not (null closest-win))
                      (values (win-geometry-id closest-win) dist)
                      nil))))))))

(defvar *search-order* (list #'win-node-down #'win-node-right #'win-node-up #'win-node-left))

(defun rotate-list (list n)
  (cond ((plusp n)
         (rotate-list
          (append (rest list) (list (first list)))
                  (1- n)))
         ((minusp n)
          (rotate-list
           (append (last list) (butlast list))
                   (1+ n)))
         (t list)))

(defun enqueue-edges (win visited queue num)
  (map nil (lambda (edge) (cl-containers:enqueue queue edge))
         (reduce (lambda (lst i)
                   ;;(let* ((func (intern (string-upcase (concat "win-node-" i)))))
                     (if (or (null (edge-target-node (funcall i win)))
                             (null (gethash (edge-target-node (funcall i win)) visited)))
                         (cons (funcall i win) lst)
                         lst));;)
                 (reverse (rotate-list *search-order* num)) :initial-value nil)))

(defun geom->corners (geom)
  "(upper-left upper-right bottom-left bottom-right)"
  (list
   (make-point
    :x (point-x (win-geometry-min geom))
    :y (point-y (win-geometry-min geom)))
   (make-point
    :x (point-x (win-geometry-max geom))
    :y (point-y (win-geometry-min geom)))
   (make-point
    :x (point-x (win-geometry-min geom))
    :y (point-y (win-geometry-max geom)))
   (make-point
    :x (point-x (win-geometry-max geom))
    :y (point-y (win-geometry-max geom)))))


(defun andmap (fn data)
  (if (= (length data) 1)
      (funcall fn (car data))
      (if (funcall fn (car data))
          (andmap fn (cdr data))
          nil)))

(defun check-point-inside-geom (point geom)
    (and (<= (point-x (win-geometry-min geom))
            (point-x point))
         (>= (point-x (win-geometry-max geom))
            (point-x point))
         (<= (point-y (win-geometry-min geom))
            (point-y point))
         (>= (point-y (win-geometry-max geom))
            (point-y point))))

;; NOTE: This is slightly complicated.
;; The following two functions check for overlap.
;; These are necessary because a distance of 0 between
;; two windows is allowed, but no less.
;; If a distance of 0 were allowed, the above function
;; could simply be changed from (< >) --> (<= =>).
(defun equal-geom (geom1 geom2)
  "Check if two geometries are equal (total overlap)"
  (and (= (point-x (win-geometry-min geom1))
          (point-x (win-geometry-min geom2)))
       (= (point-y (win-geometry-min geom1))
          (point-y (win-geometry-min geom2)))
       (= (point-x (win-geometry-max geom1))
          (point-x (win-geometry-max geom2)))
       (= (point-y (win-geometry-max geom1))
          (point-y (win-geometry-max geom2)))))

(defun equal-geom-axis (geom1 geom2 coord1 coord2)
  "Check if two geometries overlap exactly on an axis.
   A clever solution wasn't immediately apparent, so this is used."
  (and (= (funcall coord1 (win-geometry-min geom1))
          (funcall coord1 (win-geometry-min geom2)))
       (= (funcall coord1 (win-geometry-max geom1))
          (funcall coord1 (win-geometry-max geom2)))
       (or
        (and (< (funcall coord2 (win-geometry-min geom1))
                (funcall coord2 (win-geometry-max geom2)))
             (> (funcall coord2 (win-geometry-min geom1))
                (funcall coord2 (win-geometry-min geom2))))
        (and (< (funcall coord2 (win-geometry-max geom1))
                (funcall coord2 (win-geometry-max geom2)))
             (> (funcall coord2 (win-geometry-max geom1))
                (funcall coord2 (win-geometry-min geom2))))
        (and (< (funcall coord2 (win-geometry-min geom2))
                (funcall coord2 (win-geometry-max geom1)))
             (> (funcall coord2 (win-geometry-min geom2))
                (funcall coord2 (win-geometry-min geom1))))
        (and (< (funcall coord2 (win-geometry-max geom2))
                (funcall coord2 (win-geometry-max geom1)))
             (> (funcall coord2 (win-geometry-max geom2))
                (funcall coord2 (win-geometry-min geom1)))))))

(defun check-overlap (geom graph)
  ;;(format t "Checking overlap of ~a~%" geom)
  (loop for key being the hash-keys in graph using (hash-value other-node)
        for other-geom = (win-node-geometry other-node)
        for (ul ur bl br) = (geom->corners geom)
        for (other-ul other-ur other-bl other-br) = (geom->corners other-geom)
        ;; Lambda functions don't appear to work in LOOP macros, so
        ;; these are explicitly delineated.
        for overlap = (or (check-point-inside-geom ul other-geom)
                          (check-point-inside-geom ur other-geom)
                          (check-point-inside-geom bl other-geom)
                          (check-point-inside-geom br other-geom)
                          (check-point-inside-geom other-ul geom)
                          (check-point-inside-geom other-ur geom)
                          (check-point-inside-geom other-bl geom)
                          (check-point-inside-geom other-br geom))
                          ;;(equal-geom-axis geom other-geom #'point-x #'point-y)
                          ;;(equal-geom-axis geom other-geom #'point-y #'point-x)
                          ;;(equal-geom geom other-geom))
        when overlap
          do (return nil)
        finally (return geom)))

(defun make-adjacent-region (source width height side)
  ;;(dformat 0 "mar:  height: ~a width: ~a~%" height width)
  (cond
    ((equal side 'up)
     (%make-win-geometry
      :center (make-point
               :x (point-x (win-geometry-center source))
               :y (1- (round (- (point-y (win-geometry-min source))
                                (/ height 2)))))
      :min (make-point
            :x (round (- (point-x (win-geometry-center source))
                         (/ width 2)))
            :y (1- (- (point-y (win-geometry-min source))
                  height)))
      :max (make-point
            :x (round (+ (point-x (win-geometry-center source))
                         (/ width 2)))
            :y (1- (point-y (win-geometry-min source))))))
    ((equal side 'down)
     (%make-win-geometry
      :center (make-point
               :x (point-x (win-geometry-center source))
               :y (1+ (round (+ (point-y (win-geometry-max source))
                                (/ height 2)))))
      :min (make-point
            :x (round (- (point-x (win-geometry-center source))
                         (/ width 2)))
            :y (1+ (point-y (win-geometry-max source))))
      :max (make-point
            :x (round (+ (point-x (win-geometry-center source))
                         (/ width 2)))
            :y (1+ (+ (point-y (win-geometry-max source))
                      height)))))
    ((equal side 'left)
     (%make-win-geometry
      :center (make-point
               :x (1- (round (+ (point-x (win-geometry-center source))
                                (/ width 2))))
               :y (point-y (win-geometry-center source)))
      :min (make-point
            :x (1- (- (point-x (win-geometry-min source))
                      width))
            :y (round (- (point-y (win-geometry-center source))
                         (/ height 2))))
      :max (make-point
            :x (1- (point-x (win-geometry-min source)))
            :y (round (+ (point-y (win-geometry-center source))
                         (/ height 2))))))
    ((equal side 'right)
     (%make-win-geometry
      :center (make-point
               :x (1+ (round (+ (point-x (win-geometry-center source))
                            (/ width 2))))
               :y (point-y (win-geometry-center source)))
      :min (make-point
            :x (1+ (point-x (win-geometry-max source)))
            :y (round (- (point-y (win-geometry-center source))
                         (/ height 2))))
      :max (make-point
            :x (1+ (+ (point-x (win-geometry-max source))
                      width))
            :y (round (+ (point-y (win-geometry-center source))
                         (/ height 2))))))))

(defun find-window-location (geom root-win-key graph)
  "Takes a specified window geometry and finds a place for that
   window as close to the current window as possible"
  (let* ((root-node (gethash root-win-key graph))
         (queue (make-instance 'cl-containers:basic-queue))
         (current-side 0)
         (width (- (point-x (win-geometry-max geom)) (point-x (win-geometry-min geom))))
         (height (- (point-y (win-geometry-max geom)) (point-y (win-geometry-min geom)))))
    (cond
      ((not (null root-node))
       ;;(format t "Root node is not null~%")
       (enqueue-edges root-node (make-hash-table) queue current-side)
       (loop
         ;; with candidates = nil
         with visited = (make-hash-table)
         while (not (cl-containers:empty-p queue))
         for edge = (cl-containers:dequeue queue)
         ;;for null-edge = (equal (edge-dist edge) 'inf)
         for source = (win-node-geometry (gethash (edge-source-node edge) graph))
         ;; for target = (win-node-geometry (gethash (edge-source-node edge) graph))
         for place =(cond
                      ((equal (edge-dist edge) 'inf)
                       (check-overlap (make-adjacent-region source width height (edge-dir edge)) graph))
                      ((and (eq (edge-dir edge) 'up) (<= height (edge-dist edge)))
                       (check-overlap (make-adjacent-region source width height 'up) graph))
                      ((and (eq (edge-dir edge) 'down) (<= height (edge-dist edge)))
                       (check-overlap (make-adjacent-region source width height 'down) graph))
                      ((and (eq (edge-dir edge) 'left) (<= width (edge-dist edge)))
                       (check-overlap (make-adjacent-region source width height 'left) graph))
                      ((and (eq (edge-dir edge) 'right) (<= width (edge-dist edge)))
                       (check-overlap (make-adjacent-region source width height 'right) graph)))
         when (not (null place))
           collect place into candidates
         do (format t "Candidates ~a~%" candidates)
         do (format t "Edge: ~a~%" edge)
         do (incf current-side)
         do (when (not (null (edge-target-node edge)))
              (format t "Target node: ~a~%" (edge-target-node edge))
              (enqueue-edges (gethash (edge-target-node edge) graph) visited queue current-side))
         do (setf (gethash (edge-target-node edge) visited) t)
         finally (return (or candidates geom))))
      ((null root-node)
       ;;(format t "Root node is null~%")
       (list geom)))))

(defun node-side-edge (geom win-list id side)
  (multiple-value-bind (node dist)
      (locate-closest-window geom win-list (string-downcase (symbol-name side)) :level 'bounds)
    (make-edge :source-node id :target-node node :dir side :dist (or dist 'inf))))

(defun sq-euclid-distance (p q)
  (cond ((or (null p) (null q)) 0)
        (t (+ (expt (- (car p) (car q)) 2)
              (sq-euclid-distance (cdr p) (cdr q))))))

(defun euclid-distance (p q) (sqrt (sq-euclid-distance p q)))

(defun find-closest-geom (root-geom geom-list)
  "Compare geometry centers between a specified geometry and a list of
   geometries. Return the closest geometry in the list."
  (loop
    with best-dist = nil
    with best-geom = nil
    for geom in geom-list
    for dist = (euclid-distance (point->list (win-geometry-center root-geom))
                                (point->list (win-geometry-center geom)))
    ;; do (format t "Current: ~a Best: ~a ~%" dist best-dist)
    when (equal best-dist nil)
      do (setq best-dist dist
               best-geom geom)
    when (< dist best-dist)
      do (setq best-dist dist
               best-geom geom)
    finally (return best-geom)))

;; TODO
;; Make hashtable with xwin ID as the key.
;; Window node as the value.
;; Look at pan-window and find-window-to-side for filling the slots in the node.
;;
(defun new-node-in-graph (init-geom root-node graph &key win)
  (let* ((win-list (graph->geom-list graph))
         (geom-candidates (find-window-location init-geom root-node graph))
         ;;(root-geom (if (gethash root-node graph)
         ;;               (win-node-geometry (gethash root-node graph))
         ;;               (car geom-candidates)))
         ;;(geom (find-closest-geom root-geom (remove-if #'null geom-candidates)))
         (geom (car geom-candidates))
         (id (win-geometry-id init-geom)))
    (setf (win-geometry-id geom) id)
    (format t "Candidates ~S~%" geom-candidates)
    (make-win-node
     :win win
     :id id
     :geometry geom
     :up (node-side-edge geom win-list id 'up)
     :down (node-side-edge geom win-list id 'down)
     :left (node-side-edge geom win-list id 'left)
     :right (node-side-edge geom win-list id 'right))))

;; TODO Update other windows in graph
;; Probably just re-evaluate the windows on each side of the current window.
(defun update-node-edges (node graph)
  (let* ((win-list (graph->geom-list graph))
         (geom (win-node-geometry node))
         (id (win-node-id node)))
    (setf (win-node-up node) (node-side-edge geom win-list id 'up))
    (setf (win-node-down node) (node-side-edge geom win-list id 'down))
    (setf (win-node-left node) (node-side-edge geom win-list id 'left))
    (setf (win-node-right node) (node-side-edge geom win-list id 'right))))

(defun update-all-edges (graph)
  (loop
    for key being the hash-keys in graph using (hash-value node)
    do (update-node-edges node graph)))

(defun update-graphs (win)
  (with-accessors ((xwin window-xwin))
      win
    (let* ((cluster-id  (gethash (xlib:window-id xwin) *win-to-cluster*))
           (cluster (gethash cluster-id *clusters-hash*)))
      (unless (null cluster)
        (setf (win-node-geometry (gethash (xlib:window-id xwin) cluster))
              (make-win-geometry win))
        (update-all-edges cluster)))))

(add-hook *move-float-window-hook* 'update-graphs)

(defun add-win ()
  (let* ((root-node (xlib:window-id (window-xwin (if (not (null *last-focused*)) *last-focused* (current-window)))))
         (cluster (gethash root-node *win-to-cluster*))
         (graph (or (gethash cluster *clusters-hash*) (make-hash-table)))
         (new-node (new-node-in-graph
                    (make-win-geometry (current-window))
                    root-node
                    graph
                    :win (current-window)))
         (geom (win-node-geometry new-node))
         (height-diff (- (xlib:drawable-height (window-parent (current-window)))
                         (window-height (current-window))))
         (width-diff (- (xlib:drawable-width (window-parent (current-window)))
                        (window-width (current-window)))))
    (when (equal cluster nil)
      (setq cluster (1+ (hash-table-count *clusters-hash*)) )
      (setf (gethash cluster *clusters-hash*) graph))
    (setf (gethash (win-node-id new-node) graph) new-node)
    (setf (gethash (win-node-id new-node) *window-graph*) new-node)
    (setf (gethash (win-node-id new-node) *win-to-cluster*) cluster)
    (update-all-edges graph)
    (float-window-move-resize
     (win-node-win new-node)
     :x (point-x (win-geometry-min geom))
     :y (point-y (win-geometry-min geom))
     :width (- (point-x (win-geometry-max geom))
               (point-x (win-geometry-min geom))
               width-diff)
     :height (- (point-y (win-geometry-max geom))
                (point-y (win-geometry-min geom))
                height-diff)))
  (echo "Added window to graph."))

(defun add-win-wrapped ()
  (with-restarts-menu
    (add-win)))

;;(add-hook *new-window-hook* 'add-win)

;; TODO
;; Call every time a window is moved.
;; Change the window's closest neighbors.
;; Find all instances of it and update the nodes where it has changed.
;;(defun update-window-graph (win))

;; TODO
;; Call every time a window is removed from the list.
;; Remove its place in the hash table and update all nodes that have an edge to it.
(defun remove-window-from-graph (win)
  (remhash (xlib:window-id (window-xwin win)) *window-graph*)
  (update-all-edges *window-graph*))

(add-hook *destroy-window-hook* 'remove-window-from-graph)
