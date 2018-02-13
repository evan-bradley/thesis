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
;; This file zooming and big desktop functionality.
;;
;; Code:
(in-package :stumpwm)

(defvar *current-scale* 100)
(defvar *scaled-hook* '(overview-mouse-control))
(defvar *views* (make-array 10))
(defvar *current-offset* (make-point :x 0 :y 0))

;; TODO: Use an "anchor" geometry that is panned with the desktop
;; TODO: Alternatively, record the total position panned over time, and simply
;;       record that to track the absolute position in the plane.
(defstruct view
  win    ;; Reference window
  coords ;; Reference window's coordinates and time of recording
  scale) ;; Current scale

(defun scale-value (val)
  (round (* val (/ *current-scale* 100))))

(defun find-value (val lst)
  (cond
    ((null lst) 'value-not-found)
    ((eq val (car lst)) (cadr lst))
    (T (find-value val (cddr lst)))))

(defun scale-desktop (amount)
  (dbus:with-open-bus (bus (dbus:session-server-addresses))
    (dbus:with-introspected-object (compton bus "/" "com.github.chjj.compton._1_0")
      (compton "com.github.chjj.compton" "scale" (float (/ amount 100))))))

(defun pan-desktop-visual (x y)
  (incf (point-x *current-offset*) x)
  (incf (point-y *current-offset*) y)
  (dbus:with-open-bus (bus (dbus:session-server-addresses))
    (dbus:with-introspected-object (compton bus "/" "com.github.chjj.compton._1_0")
      (compton "com.github.chjj.compton" "offset" x y))))

;; Make smaller.
(defcommand scale-up (amount) ((:number "Enter scale increase: "))
  (setq *current-scale* (+ *current-scale* amount))
  (scale-desktop *current-scale*)
  ;; We just started scaling, capture the mouse.
  (if (and (= (- *current-scale* amount) 100) (not (= amount 0)))
      (run-hook *scaled-hook*)))

;; Make bigger.
(defcommand scale-down (amount) ((:number "Enter scale decrease: "))
  (cond ((not (and (= *current-scale* 100) (not (= amount 0))))
         (setq *current-scale* (- *current-scale* amount))
         (if (and (= *current-scale* 100) (not (= amount 0)))
             (ungrab-pointer))
         (scale-desktop *current-scale*))))

(defcommand reset-scale () ()
  (setq *current-scale* 100)
  (scale-desktop *current-scale*))

(defcommand record-view (number) ((:number "View number: "))
  (let* ((win (current-window))
         (coords (make-point
                  :x (window-x win)
                  :y (window-y win))))
    (setf (elt *views* (- number 1))
          (make-view
           :win win
           :coords coords
           :scale *current-scale*))))

(defcommand activate-view (number) ((:number "View number: "))
  (let* ((view (elt *views* (- number 1)))
         (win (view-win view))
         (coords (view-coords view))
         (cur-win-x (window-x win))
         (cur-win-y (window-y win)))
    (pan-group (- (point-x coords) cur-win-x)
               (- (point-y coords) cur-win-y))
    (scale-desktop (view-scale view))
    (setq *current-scale* (view-scale view))
    (when (> (view-scale view) 100)
      (run-hook *scaled-hook*))))

(defun get-bounding-box-scale (box-width box-height)
  (let* ((width (max box-width (screen-width (current-screen))))
         (height (max box-height (screen-height (current-screen))))
         (scale-width (/ width (screen-width (current-screen))))
         (scale-height (/ height (screen-height (current-screen))))
         (final-scale (* 100 (max scale-width scale-height))))
    final-scale))

(defcommand echo-bounding-box () ()
  (echo (get-list-bounding-box (group-windows (current-group)))))

(defcommand view-all-in-group () ()
  (let* ((group-extents (get-list-bounding-box (group-windows (current-group))))
         (min-coords (first group-extents))
         (max-coords (second group-extents))
         (min-x (first min-coords))
         (min-y (second min-coords))
         (max-x (first max-coords))
         (max-y (second max-coords))
         (scale (get-bounding-box-scale (- max-x min-x) (- max-y min-y))))
    (cond ((or (> (- max-x min-x) (screen-width (current-screen)))
               (> (- max-y min-y) (screen-height (current-screen))))
           (pan-group (- 0 min-x)
                      (- 0 min-y))
           (setq *current-scale* scale)
           (scale-desktop scale)
           (run-hook *scaled-hook*)))))

(defun zoom-to-window (win align)
  (let* ((extents (get-win-extents win))
         (min-coords (first extents))
         (max-coords (second extents))
         (min-x (first min-coords))
         (min-y (second min-coords))
         (max-x (first max-coords))
         (max-y (second max-coords))
         (scale (get-bounding-box-scale (- max-x min-x) (- max-y min-y)))
         (offset-x 0)
         (offset-y 0))
    (cond
      ((equal align 'left)
       (setq offset-x 0)
       (setq offset-y 0))
      ((equal align 'center)
       (if (= scale 100)
           (progn
             (setq offset-x (round (/ (- (screen-width (current-screen)) (- max-x min-x)) 2)))
             (setq offset-y (round (/ (- (screen-height (current-screen)) (- max-y min-y)) 2))))
           (progn
             (setq offset-x (- min-x max-x))
             (setq offset-y (- min-y max-y))))))
    (pan-group (- offset-x min-x)
               (- offset-y min-y))
    (setq *current-scale* scale)
    (scale-desktop scale)
    #|(if (> scale 100) (run-hook *scaled-hook*))|#))

(defcommand move-to-window (dir) ((:string "Enter direction: "))
  (if (or (equal dir "up") (equal dir "down") (equal dir "left") (equal dir "right"))
      (let* ((win-id (locate-closest-window
                      (make-win-geometry (group-current-window (current-group)))
                      (graph->geom-list *window-graph*)
                      dir))
             (win (win-node-win (gethash win-id *window-graph*))))
        (if (not (null win))
            (progn
              (zoom-to-window win 'left)
              (focus-window win t))
            (echo "No window in that direction.")))
      (echo "Invalid direction. Allowed directions: up, down, left, right.")))

(defun get-window-at-point (x y list-of-windows)
  (let ((win (car list-of-windows)))
    (cond
      ((null list-of-windows) 'root-window)
      ((and (>= x (window-x win))
            (>= y (window-y win))
            (<= x (+ (window-x win) (window-width win)))
            (<= y (+ (window-y win) (window-height win))))
       (car list-of-windows))
      (T (get-window-at-point x y (cdr list-of-windows))))))

(defcommand query-point (x y) ((:number "Enter x: ") (:number "Enter y: "))
    (echo (get-window-at-point
     (round (/ x
               (/ *current-scale* 100)))
     (round (/ y
               (/ *current-scale* 100)))
     (group-windows (current-group)))))

(defvar *zoom-map*
  (list
   (list (kbd "M-h") (list-to-string (list "pan-group " (+ 0 (head-width (current-head))) " " 0)))
   (list (kbd "M-j") (list-to-string (list "pan-group " 0 " " (- 0 (head-height (current-head))))))
   (list (kbd "M-k") (list-to-string (list "pan-group " 0 " " (+ 0 (head-height (current-head))))))
   (list (kbd "M-l") (list-to-string (list "pan-group " (- 0 (head-width (current-head))) " " 0)))
   (list (kbd "M-H") "move-to-window left")
   (list (kbd "M-J") "move-to-window down")
   (list (kbd "M-K") "move-to-window up")
   (list (kbd "M-L") "move-to-window right"))
  "Keyamp set when in a zooming state for interacting with and manipulating windows.")

(defun toggle-zoom-minor-mode (enable)
  (if enable
      (map 'nil (lambda (zoom-binding) (define-key
                                      *top-map*
                                      (first zoom-binding)
                                    (second zoom-binding))) *zoom-map*)
      (map 'nil (lambda (zoom-binding) (undefine-key
                                      *top-map*
                                      (first zoom-binding))) *zoom-map*)))

(defun pan-centered-closure ()
  (let ((offset-x 0) (offset-y 0))
    (lambda ()
      (let* ((current-scale (/ *current-scale* 100))
             (width (* (head-width (current-head)) current-scale))
             (height (* (head-height (current-head)) current-scale))
             (cur-offset-x (round (-
                                   (/ width 2)
                                   (/ width
                                      (* current-scale 2)))))
             (cur-offset-y (round (-
                                   (/ height 2)
                                   (/ height
                                      (* current-scale 2))))))
        ;;(echo (list (- cur-offset-x offset-x) (- cur-offset-y offset-y)))
        (pan-group (- cur-offset-x offset-x)
                   (- cur-offset-y offset-y))
        (setq offset-x cur-offset-x)
        (setq offset-y cur-offset-y)))))

(defvar pan-centered (pan-centered-closure))

(defun overview-mouse-control ()
  (toggle-zoom-minor-mode t)
  (let ((op nil) (cur-win nil) (prev-x 0) (prev-y 0) (win-x-diff 0) (win-y-diff 0))
   (labels ((overview-mouse-event-handler
               (&rest event-slots &key event-key &allow-other-keys)
             ;; MASKS (in event-slots):
             ;; left : 256
             ;; Middle : 512
             ;; Right: 1024
             ;; Scroll up: 2048
             ;; Scroll down: 4096
             ;;(if (= *current-scale* 100)
             ;;    (ungrab-pointer))
              (cond
                ((= *current-scale* 100) :done)
                (T
                 (case event-key
                   (:button-press
                    (setq prev-x (getf event-slots :x))
                    (setq prev-y (getf event-slots :y))
                    (setq cur-win
                          (get-window-at-point
                           (+ (round (scale-value (getf event-slots :x)))
                              (point-x *current-offset*))
                           (+ (round (scale-value (getf event-slots :y)))
                              (point-y *current-offset*))
                           (group-windows (current-group))))
                    (cond ((and (not (equal cur-win nil)) (not (equal cur-win 'root-window)))
                           (setq win-x-diff (- (scale-value (getf event-slots :x))
                                               (window-x cur-win)))
                           (setq win-y-diff (- (scale-value (getf event-slots :y))
                                               (window-y cur-win)))))
                    (let ((mask (find-value :CODE event-slots)))
                      (case mask
                        (1 (setq op 'move) t)
                        (2 (zoom-to-window cur-win 'left) (focus-window cur-win) t)
                        (3 (setq op 'resize) t)
                        (4 (scale-down (min 6 (- *current-scale* 100)))
                         ;;(funcall pan-centered)
                         (if (= *current-scale* 100)
                             :done
                             t))
                        (5 (scale-up 6) #|(funcall pan-centered)|# t))))
                   (:button-release
                    (unless (and (= 0 (point-x *current-offset*))
                                 (= 0 (point-y *current-offset*)))
                      (pan-desktop-visual 0 0)
                      (pan-group (point-x *current-offset*)
                                 (point-y *current-offset*))
                      (setf (point-x *current-offset*) 0)
                      (setf (point-y *current-offset*) 0))
                    (setq op nil)
                    (setq cur-win nil)
                    (setq win-x-diff 0)
                    (setq win-y-diff 0)
                    t)
                   (:motion-notify
                    (let ((cur-x (getf event-slots :x))
                          (cur-y (getf event-slots :y))
                          (diff-x (- (getf event-slots :x) prev-x))
                          (diff-y (- (getf event-slots :y) prev-y)))
                      (cond
                        ((equal cur-win nil) t)
                        ((equal cur-win 'root-window)
                         ;; Multiply by two to make motion easier.
                         ;;
                         (pan-desktop-visual
                          (round (* (* 2 diff-x) (/ *current-scale* 100)))
                          (round (* (* 2 diff-y) (/ *current-scale* 100)))))
                        (T
                         (case op
                           (move (float-window-move-resize
                                  cur-win
                                  :x (- (scale-value cur-x) win-x-diff)
                                  :y (- (scale-value cur-y) win-y-diff)))
                           (resize (float-window-move-resize
                                    cur-win
                                    :width (+ (window-width cur-win) (scale-value diff-x))
                                    :height (+ (window-height cur-win) (scale-value diff-y)))))
                         (setf (window-x cur-win) (xlib:drawable-x (window-parent cur-win))
                               (window-y cur-win) (xlib:drawable-y (window-parent cur-win)))
                         (run-hook-with-args *move-float-window-hook* cur-win)))
                      (setq prev-x cur-x)
                      (setq prev-y cur-y))
                    t)
                   ;; We need to eat these events or they'll ALL
                   ;; come blasting in later. Also things start
                   ;; lagging hard if we don't (on clisp anyway).
                   (:configure-notify t)
                   (:exposure t)
                   (:key-press
                    (let ((eventfn (gethash :key-press *event-fn-table*)))
                      (apply eventfn event-slots))
                    t)
                   (t nil))))))
    (xlib:grab-pointer (screen-root (current-screen))
                       '(:button-press
                         :button-release
                         :pointer-motion))
    (unwind-protect
         ;; Wait until the mouse button is released
         (loop for ev = (xlib:process-event *display*
                                            :handler #'overview-mouse-event-handler
                                            :timeout nil
                                            :discard-p t)
               until (eq ev :done))
      (ungrab-pointer)))))

;; TODO State mask
;; (multiple-value-bind (relx rely same-screen-p child state-mask)
;;    (xlib:query-pointer (window-parent window))
(defun root-window-click (screen button x y)
  (declare (ignore button))
  (let ((prev-x x) (prev-y y) (zoomed nil))
    (labels ((pan-screen-event-handler
                 (&rest event-slots &key event-key &allow-other-keys)
               ;; TODO: Use this for modifier support.
               ;; Returns (e.g.): (BUTTON-1 MOD4)
               ;;(echo (xlib:make-state-keys (find-value :STATE event-slots)))
               (let ((mask (find-value :CODE event-slots)))
                 (case mask
                   ;; Pan desktop (plus move window?).
                   (1 'root-window-click-left)
                   ;; Reset scale?
                   (2 'root-window-click-middle)
                   ;; Check window position and move it according to scale.
                   (3 'root-window-click-right)
                   (4 (scale-down 6) #|(funcall pan-centered)|# (setq zoomed t))
                   (5 (scale-up 6) #|(funcall pan-centered)|# (setq zoomed t))))

               (if (not zoomed)
                   (case event-key
                     (:button-release :done)
                     (:motion-notify
                      (let ((cur-x (getf event-slots :x))
                            (cur-y (getf event-slots :y)))
                        ;; Multiply by two to make motion easier.
                        (pan-group (* 2 (- cur-x prev-x))
                                   (* 2 (- cur-y prev-y)))
                        (setq prev-x cur-x)
                        (setq prev-y cur-y))
                      t)
                     ;; We need to eat these events or they'll ALL
                     ;; come blasting in later. Also things start
                     ;; lagging hard if we don't (on clisp anyway).
                     (:configure-notify t)
                     (:exposure t)
                     (t nil))
                   :done)))
      (xlib:grab-pointer (screen-root screen) '(:button-release :pointer-motion))
      (unwind-protect
           ;; Wait until the mouse button is released
           (loop for ev = (xlib:process-event *display*
                                              :handler #'pan-screen-event-handler
                                              :timeout nil
                                              :discard-p t)
                 until (eq ev :done))
        (ungrab-pointer)))))

(add-hook *root-click-hook* 'root-window-click)
;;(remove-hook *root-click-hook* 'root-window-click)

