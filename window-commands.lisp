;; Copyright (C) 2003-2008 Shawn Betts
;;
;;  This file is part of thesiswm.
;;
;; thesiswm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; thesiswm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; Window commands functionality.
;; This was moved into its own file since it uses functions from zooming.lisp.
;;
;; Code:

(in-package #:thesiswm)

(defcommand delete-window (&optional (window (current-window))) ()
  "Delete a window. By default delete the current window. This is a
request sent to the window. The window's client may decide not to
grant the request or may not be able to if it is unresponsive."
  (when (find window *always-show-windows*)
    (disable-always-show-window window (current-screen)))
  (when window
    (send-client-message window :WM_PROTOCOLS (xlib:intern-atom *display* :WM_DELETE_WINDOW))))

(defcommand-alias delete delete-window)

(defcommand kill-window (&optional (window (current-window))) ()
  "Tell X to disconnect the client that owns the specified
window. Default to the current window. if
@command{delete-window} didn't work, try this."
  (when window
    (xwin-kill (window-xwin window))))

(defcommand-alias kill kill-window)

(defcommand title (title) ((:rest "Set window's title to: "))
  "Override the current window's title."
  (if (current-window)
      (setf (window-user-title (current-window)) title)
      (message "No Focused Window")))

(defcommand select-window (query) ((:window-name "Select: "))
  "Switch to the first window that starts with @var{query}."
  (let (match)
    (labels ((match (win)
               (let* ((wname (window-name win))
                      (end (min (length wname) (length query))))
                 (string-equal wname query :end1 end :end2 end))))
      (unless (null query)
        (setf match (find-if #'match (group-windows (current-group)))))
      (when match
        (group-focus-window (current-group) match)))))

(defcommand-alias select select-window)

(defcommand select-window-by-name (name) ((:window-name "Select: "))
  "Switch to the first window whose name is exactly @var{name}."
  (let ((win (find name (group-windows (current-group))
                   :test #'string= :key #'window-name)))
    (when win
      (group-focus-window (current-group) win))))

(defcommand select-window-by-number (num &optional (group (current-group)))
                                    ((:window-number "Select: "))
  "Find the window with the given number and focus it in its frame."
  (labels ((match (win)
             (= (window-number win) num)))
    (let ((win (find-if #'match (group-windows group))))
      (when win
        (group-focus-window group win)))))

(defcommand other-window (&optional (group (current-group))) ()
  "Switch to the window last focused."
  (let* ((wins (only-tile-windows (group-windows group)))
         ;; the frame could be empty
         (win (if (group-current-window group)
                  (second wins)
                  (first wins))))
    (if win
        (group-focus-window group win)
        (echo-string (group-screen group) "No other window."))))

(defcommand-alias other other-window)

(defcommand renumber (nt &optional (group (current-group))) ((:number "Number: "))
  "Change the current window's number to the specified number. If another window
is using the number, then the windows swap numbers. Defaults to current group."
  (let ((nf (window-number (group-current-window group)))
        (win (find-if #'(lambda (win)
                          (= (window-number win) nt))
                      (group-windows group))))
    ;; Is it already taken?
    (if win
        (progn
          ;; swap the window numbers
          (setf (window-number win) nf)
          (setf (window-number (group-current-window group)) nt))
        ;; Just give the window the number
        (setf (window-number (group-current-window group)) nt))))

(defcommand-alias number renumber)

(defcommand repack-window-numbers (&optional preserved) ()
  "Ensure that used window numbers do not have gaps; ignore PRESERVED window numbers."
  (let* ((group (current-group))
	 (windows (sort-windows group)))
    (loop for w in windows
	  do (unless (find (window-number w) preserved)
	       (setf
		 (window-number w)
		 (find-free-number
		   (remove
		     (window-number w)
		     (mapcar 'window-number windows))
		   0))))))

;; It would make more sense that the window-list argument was before the fmt one
;; but window-list was added latter and I didn't want to break other's code.
(defcommand windowlist (&optional (fmt *window-format*)
                                  window-list) (:rest)
  "Allow the user to select a window from the list of windows and focus the
selected window. For information of menu bindings see @ref{Menus}. The optional
 argument @var{fmt} can be specified to override the default window formatting.
The optional argument @var{window-list} can be provided to show a custom window
list (see @command{windowlist-by-class}). The default window list is the list of
all window in the current group. Also note that the default window list is sorted
by number and if the @var{windows-list} is provided, it is shown unsorted (as-is)."
  ;; Shadowing the window-list argument.
  (let ((window-list (or window-list
                         (sort-windows-by-number
                          (group-windows (current-group))))))
    (if (null window-list)
        (message "No Managed Windows")
        (let ((window (select-window-from-menu window-list fmt)))
          (if window
              (progn
                (when (typep window 'float-window)
                    (zoom-to-window window 'center))
                (group-focus-window (current-group) window))
              (throw 'error :abort))))))


(defcommand windowlist-by-class (&optional (fmt *window-format-by-class*)) (:rest)
  "Allow the user to select a window from the list of windows (sorted by class)
 and focus the selected window. For information of menu bindings see @ref{Menus}.
The optional argument @var{fmt} can be specified to override the default window
formatting. This is a simple wrapper around the command @command{windowlist}."
  (windowlist fmt (sort-windows-by-class (group-windows (current-group)))))

(defcommand window-send-string (string &optional (window (current-window))) ((:rest "Insert: "))
  "Send the string of characters to the current window as if they'd been typed."
  (when window
    (map nil (lambda (ch)
               ;; exploit the fact that keysyms for ascii characters
               ;; are the same as their ascii value.
               (let ((sym (cond ((<= 32 (char-code ch) 127)
                                 (char-code ch))
                                ((char= ch #\Tab)
                                 (thesiswm-name->keysym "TAB"))
                                ((char= ch #\Newline)
                                 (thesiswm-name->keysym "RET"))
                                (t (first (xlib:character->keysyms ch *display*))))))
                 (when sym
                   (send-fake-key window
                                  (make-key :keysym sym)))))
         string)))

(defcommand-alias insert window-send-string)

(defcommand mark () ()
"Toggle the current window's mark."
  (let ((win (current-window)))
    (when win
      (setf (window-marked win) (not (window-marked win)))
      (message (if (window-marked win)
                   "Marked!"
                   "Unmarked!")))))

(defcommand clear-window-marks (&optional (group (current-group)) (windows (group-windows group))) ()
"Clear all marks in the current group."
  (dolist (w windows)
    (setf (window-marked w) nil)))

(defcommand-alias clear-marks clear-window-marks)

(defcommand echo-windows (&optional (fmt *window-format*) (group (current-group)) (windows (group-windows group))) (:rest)
  "Display a list of managed windows. The optional argument @var{fmt} can
be used to override the default window formatting."
  (let* ((wins (sort1 windows '< :key 'window-number))
         (highlight (position (group-current-window group) wins))
         (names (mapcar (lambda (w)
                          (format-expand *window-formatters* fmt w)) wins)))
    (if (null wins)
        (echo-string (group-screen group) "No Managed Windows")
        (echo-string-list (group-screen group) names highlight))))

(defcommand-alias windows echo-windows)

(defcommand info (&optional (fmt *window-info-format*)) (:rest)
  "Display information about the current window."
  (if (current-window)
      (message "~a" (format-expand *window-formatters* fmt (current-window)))
      (message "No Current Window")))

(defcommand refresh () ()
  "Refresh current window without changing its size."
  (let* ((window (current-window))
         (w (window-width window))
         (h (window-height window)))
    (set-window-geometry window
                         :width (- w (window-width-inc window))
                         :height (- h (window-height-inc window)))
    ;; make sure the first one goes through before sending the second
    (xlib:display-finish-output *display*)
    (set-window-geometry window
                         :width w
                         :height h)))

(defcommand toggle-always-on-top () ()
  "Toggle whether the current window always appears over other windows.
The order windows are added to this list determines priority."
  (let ((w (current-window))
        (windows (group-on-top-windows (current-group))))
    (when w
      (if (find w windows)
          (setf (group-on-top-windows (current-group)) (remove w windows))
          (push (current-window) (group-on-top-windows (current-group)))))))
