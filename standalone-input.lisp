;; Copyright (C) 2003-2008 Shawn Betts
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
;; This file sloppily makes the input bar modular.
;; TODO: Make two input strings for the bar.
;;       One contains the actual input in the bar, while the other is a representation
;;       changed to make the output nicer, which is the version displayed on the screen.
;;       e.g. backend string: [[pan 1920 0][right]] [[pan -1920 0][left]]
;;           frontend string: right left
;;       The frontend string will be operated on most of the time, except for when
;;       certain formatting is broken, after which portions will be replaced by the
;;       backend string as needed, which will contain all formatting cues.
;;       This should work a lot like links in org-mode.
;;
;; Code:
(in-package :stumpwm)

(export '(make-input-bar
          completing-bar-read))

(defvar *default-command-prompt*
  "λ ")

(defvar *default-command-string*
  "{[delete] x} {[maximize] ^} {[left] <} {[right] >}")

(defvar *default-command-list*
  '(((eval-command "delete") "x")
    ((eval-command "maximize 1 1") "^")
    ((eval-command "left") "<")
    ((eval-command "right") ">")))

(defstruct string-pos-assoc
  input output command)

(defstruct input-bar-line
  string position selection history history-bk password output-assoc)

(defstruct (input-bar (:constructor %make-input-bar))
  screen
  parent
  window
  key-window
  message-cc
  input-line
  fonts
  key-map
  history
  last-command
  completions
  current-completions
  current-completions-idx
  history-ignore-duplicates
  numpad-map)

(defvar *input-bars* ()
  "All current input bars.")

(defun make-input-bar (&key parent (initial-input "") password)
  (let* ((bar nil)
         (screen-number (screen-number (current-screen)))
         (default-colormap (xlib:screen-default-colormap screen-number))
         (screen-root (xlib:screen-root screen-number))
         (parent-window (or parent screen-root))
         (fg-color (screen-fg-color (current-screen)))
         (bg-color (screen-bg-color (current-screen)))
         (border-color (screen-border-color (current-screen)))
         (font (screen-font (current-screen)))
           ;;(open-font *display*
               ;;           (if (font-exists-p +default-font-name+)
               ;;               +default-font-name+
               ;;               "*")))
         (message-window (xlib:create-window :parent parent-window
                                             :x 0 :y 0 :width 1 :height 1
                                             :colormap default-colormap
                                             :background bg-color
                                             :border border-color
                                             :border-width 1
                                             :bit-gravity :north-west
                                             :event-mask '(:exposure))))
    (setq bar (%make-input-bar
               :screen (current-screen)
               :parent parent-window
               :window (xlib:create-window
                        :parent parent-window
                        :x 0 :y 0 :width 20 :height 20
                        :colormap default-colormap
                        :background bg-color
                        :border border-color
                        :border-width 1
                        :event-mask '(:focus-change :button-press :button-release :key-press :key-release))
               :key-window (xlib:create-window
                            :parent parent-window
                            :x 0 :y 0 :width 1 :height 1
                            :event-mask '(:key-press :key-release))
               :message-cc (make-ccontext
                            :win message-window
                            :font font
                            :gc (xlib:create-gcontext
                                 :drawable message-window
                                 :font (when (typep font 'xlib:font) font)
                                 :foreground fg-color
                                 :background bg-color))
               :input-line (make-input-bar-line :string (make-input-bar-string initial-input)
                                                :position (length initial-input)
                                                :history -1
                                                :password password
                                                :output-assoc '())
               :fonts (list font)
               :key-map (make-sparse-keymap) ;;nil
               :history nil
               :last-command nil
               :completions nil
               :current-completions nil
               :current-completions-idx nil
               :history-ignore-duplicates nil
               :numpad-map '((87 10 . 16) (88  11 . 16) (89 12 . 16) (106 61 . 16)
                             (83 13 . 16) (84  14 . 16) (85 15 . 16) (86  21 . 17)
                             (79 16 . 16) (80  17 . 16) (81 18 . 16) (63  17 . 17)
                             (82 20 . 16) (104 36 . 16) (91 60 . 16) (90  19 . 16))))
    (xlib:change-property
     (input-bar-window bar)
     :_NET_WM_WINDOW_TYPE
     (list (xlib:find-atom *display* :_NET_WM_WINDOW_TYPE_DESKTOP))
     :atom
     32)
    (define-key (input-bar-key-map bar) (kbd "ESC") 'input-bar-abort)
    ;; FIXME: Keys are disabled until editing is finalized.
    #|(when (null (input-bar-key-map bar))
      (setf (input-bar-key-map bar)
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "DEL") 'input-bar-delete-backward-char)
         (define-key map (kbd "M-DEL") 'input-bar-backward-kill-word)
         (define-key map (kbd "C-d") 'input-bar-delete-forward-char)
         (define-key map (kbd "M-d") 'input-bar-forward-kill-word)
         (define-key map (kbd "Delete") 'input-bar-delete-forward-char)
         (define-key map (kbd "C-f") 'input-bar-forward-char)
         (define-key map (kbd "Right") 'input-bar-forward-char)
         (define-key map (kbd "M-f") 'input-bar-forward-word)
         (define-key map (kbd "C-b") 'input-bar-backward-char)
         (define-key map (kbd "Left") 'input-bar-backward-char)
         (define-key map (kbd "M-b") 'input-bar-backward-word)
         (define-key map (kbd "C-a") 'input-bar-move-beginning-of-line)
         (define-key map (kbd "Home") 'input-bar-move-beginning-of-line)
         (define-key map (kbd "C-e") 'input-bar-move-end-of-line)
         (define-key map (kbd "End") 'input-bar-move-end-of-line)
         (define-key map (kbd "C-k") 'input-bar-kill-line)
         (define-key map (kbd "C-u") 'input-bar-kill-to-beginning)
         ;; DISABLED: Rely on global state.
         (define-key map (kbd "C-p") 'input-bar-history-back)
         (define-key map (kbd "Up") 'input-bar-history-back)
         (define-key map (kbd "C-n") 'input-bar-history-forward)
         (define-key map (kbd "Down") 'input-bar-history-forward)
         (define-key map (kbd "M-Right") 'input-bar-space-delimit)
         (define-key map (kbd "M-SPC") 'input-bar-bracket-delimit)
         (define-key map (kbd "RET") 'input-bar-submit)
         (define-key map (kbd "C-g") 'input-bar-abort)
         (define-key map (kbd "ESC") 'input-bar-abort)
         ;; DISABLED: Rely on global state.
         (define-key map (kbd "C-y") 'input-bar-yank-selection)
         (define-key map (kbd "C-Y") 'input-bar-yank-clipboard)
         (define-key map (kbd "TAB") 'input-bar-complete-forward)
         (define-key map (kbd "ISO_Left_Tab") 'input-bar-complete-backward)
         (define-key map t 'input-bar-self-insert)
         map)))|#
    (push bar *input-bars*)
    bar))

(defun find-input-bar-by-window (xwin)
  (find xwin *input-bars* :key #'input-bar-window))

;;; line and key reading functions

(defun input-bar-font (bar)
  (first (input-bar-fonts bar)))

(defun setup-input-bar-window (bar prompt input)
  "Set the input-bar window up to read input"
  (let* ((height (font-height (input-bar-font bar)))
         (win (input-bar-window bar)))
    ;; Window dimensions
    (xlib:with-state (win)
      (setf (xlib:window-priority win) :above
            (xlib:drawable-height win) height))
    (xlib:map-window win)
    ;; Draw the prompt
    (draw-input-bar-bucket bar prompt input)
    ;; Ready to recieve input
))

(defun shutdown-input-bar-window (bar)
  (xlib:ungrab-keyboard *display*)
  (let* ((input (input-bar-input-line bar)))
    (setf (input-bar-line-position input) -1)
    (draw-input-bar-bucket bar *default-command-prompt* input)))

;; Hack to avoid clobbering input from numpads with numlock on.
(defun input-bar-handle-key-press-event (&rest event-slots
                                     &key event-key root code state
                                       &allow-other-keys)
  (declare (ignore event-slots root))
  (let ((numlock-on-p (= 2 (logand 2 (nth-value 4 (xlib:keyboard-control *display*)))))
         (numpad-key (assoc code *numpad-map*)))
    (when (and numlock-on-p numpad-key)
      (setf code (first (rest numpad-key))
            state (rest (rest numpad-key))))
    (list* event-key code state)))

(defun input-bar-handle-selection-event (&key window selection property &allow-other-keys)
  (declare (ignore selection))
  (if property
      (xlib:get-property window property :type :string :result-type 'string :transform #'xlib:card8->char :delete-p t)
      ""))

(defun read-key-handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
  (case event-key
    ((or :key-release :key-press)
     (apply 'input-bar-handle-key-press-event event-slots))
    (t nil)))

(defun read-event-handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
  ;;(dformat 0 ">>> ~S~%" event-slots)
  ;;(dformat 0 ">>> ~S~%" event-key)
  (case event-key
    (:button-press
     ;;(dformat 0 ">>> (~a, ~a)~%" (getf event-slots :x) (getf event-slots :y))
     ;;(dformat 0 ">>> code: ~a~%" (getf event-slots :code))
     (list :type :button-press
           :x (getf event-slots :x)
           :y (getf event-slots :y)
           :code (getf event-slots :code)))
    (:button-release t)
    (:enter-notify
     (throw :abort nil)
     ;;(dformat 0 ">>> ENTERING ======~%")
     )
    (:leave-notify
     (dformat 0 ">>> LEAVING ======~%"))
    ((or :key-release :key-press)
     (apply 'input-bar-handle-key-press-event event-slots))
    (:selection-notify
     (apply 'input-bar-handle-selection-event event-slots))
    (t nil)))

(defun read-event ()
  (loop for ev = (xlib:process-event *display* :handler #'read-event-handle-event :timeout nil) do
    (cond ((stringp ev)
           (return ev))
          ((and (consp ev)
                (eq (first ev) :key-press))
           (return (rest ev)))
          ((and (consp ev)
                (eq (second ev) :button-press))
           (return ev)))))

(defun make-input-bar-string (initial-input)
  (make-array (length initial-input) :element-type 'character :initial-contents initial-input
              :adjustable t :fill-pointer t))

(defun completing-bar-read (bar prompt completions &key require-match code x y)
  "Read a line of input through stumpwm and return it with TAB
completion. completions can be a list, an fbound symbol, or a
function. if its an fbound symbol or a function then that function is
passed the substring to complete on and is expected to return a list
of matches. If require-match argument is non-nil then the input must
match with an element of the completions."
  (check-type completions (or list function symbol))
  (setf (input-bar-completions bar) completions)
  (setf (input-bar-current-completions bar) nil)
  (setf (input-bar-current-completions-idx bar) nil)
  (let ((input (input-bar-input-line bar))
        (key (list :type :button-press :x x :y y :code code))
        (should-exec t))
    (when (and code x y)
      (setq should-exec (not (catch :abort (process-key bar prompt input key)))))
    (when should-exec
      (let ((line (read-one-bar-line bar prompt input :require-match require-match)))
        (when line (string-trim " " line))))))

(defun match-input (bar input)
  (let* ((in (string-trim " " (input-bar-line-string input)))
         (compls (input-bar-find-completions in (input-bar-completions bar))))
    (and (consp compls)
         (string= in (if (consp (car compls))
                         (caar compls)
                         (car compls))))))

(defun process-key (bar prompt input key &key require-match)
  (cond
    ((and (listp key) (eq (car key) :type) (eq (getf key :type) :button-press))
     ;; The input positions start at ~27px to the right, and are 9px wide.
     (let*
         ;; TODO: Properly calculate this.
         ((pos (round (/ (max 0 (- (getf key :x) 27)) 9)))
          (cmd (input-bar-search-command input pos))
          (prompt-width (text-line-width (input-bar-font bar) prompt :translate #'translate-id)))
       (case (getf key :code)
         (1
          (if (< (getf key :x) prompt-width)
              (group-button-press
               (current-group)
               (getf key :x)
               (getf key :y)
               (group-current-window (current-group)))
              (input-bar-goto-char input pos)))
         (2
          (unless (eq cmd nil)
            (eval cmd)
            ;;(shutdown-input-bar-window bar)
            (throw :abort t)))
         (3 ;; TODO: Deduplicate this.
          (if (< (getf key :x) prompt-width)
              (group-button-press
               (current-group)
               (getf key :x)
               (getf key :y)
               (group-current-window (current-group)))
              (input-bar-goto-char input pos)))))
     (draw-input-bar-bucket bar prompt input))
    ((stringp key)
     ;; handle selection
     (input-bar-insert-string input key)
     (draw-input-bar-bucket bar prompt input))
    ;; skip modifiers
    ((is-modifier (car key)))
    ((input-bar-process bar prompt input (car key) (cdr key))
     (if (or (not require-match)
             (match-input bar input))
         (input-bar-line-selection input)
         (draw-input-bar-bucket bar prompt input "[No match]" t)))))

(defun read-one-bar-line (bar prompt input &key require-match)
  "Read a line of input through stumpwm and return it. returns nil if the user aborted."
  (setf (input-bar-last-command bar) nil)
    (labels ((key-loop ()
               (loop for key = (read-event) do
                 (process-key bar prompt input key :require-match require-match))))
      (setup-input-bar-window bar prompt input)
      (catch :abort
        (unwind-protect
             (with-focus (input-bar-window bar)
               (key-loop))
          (shutdown-input-bar-window bar)))))

(defun process-formatted-command (input-string)
  "Process a command selected by a PCRE. We can make assumptions about the string
   since it was passed in through a regex match."
    (let* ((p1 (position-if (lambda (x) (string-equal x "{"))
                            input-string :start 0))
           (p2 (and p1 (position-if (lambda (x) (string-equal x "}"))
                                    input-string :start 0)))
           (cmd-p1 (and p1 (position-if (lambda (x) (string-equal x "["))
                                        input-string :start p1)))
           (cmd-p2 (and p2 (position-if (lambda (x) (string-equal x "]"))
                                        input-string :end p2 :from-end t)))
           (command (and cmd-p1 cmd-p2 (subseq input-string (+ cmd-p1 1) cmd-p2)))
           (text (and p2 cmd-p2 (subseq input-string (+ cmd-p2 2) p2))))
      (list command text)))

(defun paste-into-string (str insertion start end)
  "Cuts the substring from start to end and inserts the insertion string."
  (if (= 0 start)
      (concat insertion (subseq str end (length str)))
      (concat (subseq str 0 start) insertion (subseq str end (length str)))))

(defun format-input-string (input-string)
  "Format strings to allow for characters to stand in for commands"
  (if (not (string-equal input-string ""))
      (loop with new-string = input-string
            with new-translation = nil
            with input-string-pos = 0
            with new-string-start = 0
            for (start end) = (multiple-value-list
                               (cl-ppcre:scan
                                "\{\\[[a-zA-Z0-9\- ]*\\] [^\{\}]*}"
                                input-string
                                :start input-string-pos))
            while (and (numberp start) (numberp end))
            do (let* ((separated (process-formatted-command
                                  (subseq input-string start end)))
                      (command (car separated))
                      (text (cadr separated)))
                 ;;(format t "new string: ~a ~%" new-string)
                 ;;(format t "new: ~a pos: ~a start: ~a end: ~a ~%"
                 ;;        new-string-start input-string-pos start end)
                 (setq new-string-start (+ new-string-start (- start input-string-pos)))
                 (setq input-string-pos end)
                 ;;(format t "new: ~a pos: ~a start: ~a end: ~a ~%"
                 ;;        new-string-start input-string-pos start end)
                 (setq new-translation (cons (make-string-pos-assoc
                                              :input (list start end)
                                              :output (list
                                                       new-string-start
                                                       (+ new-string-start (- (length text) 1)))
                                              :command command) new-translation))
                 (setq new-string (paste-into-string
                                   new-string text
                                   new-string-start
                                   (+ new-string-start (- end start))))
                 (setq new-string-start (+ new-string-start (length text))))
            finally (return (list new-string new-translation)))
      (list "" nil)))

(defun render-bar-string (command-list)
  (reduce (lambda (str item)
            (concat str
                    (if (equal str "") "" " ")
                    (second item)))
          command-list :initial-value ""))

(defun draw-input-bar-bucket (bar prompt input &optional (tail "") errorp)
  "Draw the contents of input to the input bar window."
  (let* ((gcontext (ccontext-gc (input-bar-message-cc bar)))
         (win (input-bar-window bar))
         (prompt-width (text-line-width (input-bar-font bar) prompt :translate #'translate-id))
         ;;(line-content (input-bar-line-string input))
         ;;(unformatted-string (if (input-bar-line-password input)
         ;;            (make-string (length line-content) :initial-element #\*)
         ;;            line-content))
         ;; (input-output-hash input)
         (string (render-bar-string *default-command-list*))
         (string-width (loop for char across string
                             summing (text-line-width (input-bar-font bar)
                                                      (string char)
                                                      :translate #'translate-id)))
         (space-width  (text-line-width (input-bar-font bar) " "    :translate #'translate-id))
         (tail-width   (text-line-width (input-bar-font bar) tail   :translate #'translate-id))
         (full-string-width (+ string-width space-width))
         (pos (input-bar-line-position input))
         ;; TODO: Remove padding?
         (width (max (- (window-width (current-window)) (* *message-window-padding* 2))
                   (+ prompt-width (+ full-string-width space-width tail-width)))))

    (setf (input-bar-line-string input) string)
    (if (> pos (- (length string) 1))
        (setf (input-bar-line-position input) -1))
    (when errorp (rotatef (xlib:gcontext-background gcontext)
                          (xlib:gcontext-foreground gcontext)))
    (xlib:with-state (win)
      (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background gcontext))
        (xlib:draw-rectangle win gcontext 0 0
                             (xlib:drawable-width win)
                             (xlib:drawable-height win) t))
      (setf (xlib:drawable-width win) (+ width (* *message-window-padding* 2)))
      (setup-win-gravity (input-bar-screen bar) win :TOP-LEFT))
    (xlib:with-state (win)
      (draw-image-glyphs win gcontext (input-bar-font bar)
                         *message-window-padding*
                         (font-ascent (input-bar-font bar))
                         prompt
                         :translate #'translate-id
                         :size 16)
      (loop with x = (+ *message-window-padding* prompt-width)
            for char across string
            for i from 0 below (length string)
            for char-width = (text-line-width (input-bar-font bar) (string char) :translate #'translate-id)
            if (= pos i)
              do (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background gcontext)
                                               :background (xlib:gcontext-foreground gcontext))
                   (draw-image-glyphs win gcontext (input-bar-font bar)
                                      x
                                      (font-ascent (input-bar-font bar))
                                      (string char)
                                      :translate #'translate-id
                                      :size 16))
            else
              do (draw-image-glyphs win gcontext (input-bar-font bar)
                                    x
                                    (font-ascent (input-bar-font bar))
                                    (string char)
                                    :translate #'translate-id
                                    :size 16)
            end
            do (incf x char-width)
            finally (when (>= pos (length string))
                      (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background gcontext)
                                                    :background (xlib:gcontext-foreground gcontext))
                        (draw-image-glyphs win gcontext (input-bar-font bar)
                                           x
                                           (font-ascent (input-bar-font bar))
                                           " "
                                           :translate #'translate-id
                                           :size 16))))
      (draw-image-glyphs win gcontext (input-bar-font bar)
                         (+ *message-window-padding* prompt-width full-string-width space-width)
                         (font-ascent (input-bar-font bar))
                         tail
                         :translate #'translate-id
                         :size 16))
    (when errorp
      (sleep 0.05)
      (rotatef (xlib:gcontext-background gcontext)
               (xlib:gcontext-foreground gcontext))
      (draw-input-bar-bucket bar prompt input tail))))


;;; input string utility functions

(defun input-bar-submit (bar input key)
  (declare (ignore input key) (ignore bar))
  :done)

(defun input-bar-abort (bar input key)
  (declare (ignore input key) (ignore bar))
  (throw :abort nil))

(defun input-bar-goto-char (input point)
  "Move the cursor to the specified point in the string"
  (setf (input-bar-line-position input) (min (max 0 point)
                                         (length (input-bar-line-string input)))))

(defun input-bar-insert-string (input string)
  "Insert @var{string} into the input at the current
position. @var{input} must be of type @var{input-bar-line}. Input
functions are passed this structure as their first argument."
  (check-type string string)
  (loop for c across string
	do (input-bar-insert-char input c)))

(defun input-bar-point (input)
  "Return the position of the cursor."
  (check-type input input-bar-line)
  (input-bar-line-position input))

(defun input-bar-validate-region (input start end)
  "Return a value pair of numbers where the first number is < the
second and neither excedes the bounds of the input string."
  (values (max 0 (min start end))
          (min (length (input-bar-line-string input))
               (max start end))))

(defun input-bar-delete-region (input start end)
  "Delete the region between start and end in the input string"
  (check-type input input-bar-line)
  (check-type start fixnum)
  (check-type end fixnum)
  (multiple-value-setq (start end) (input-bar-validate-region input start end))
  (replace (input-bar-line-string input) (input-bar-line-string input)
           :start2 end :start1 start)
  (decf (fill-pointer (input-bar-line-string input)) (- end start))
  (cond
    ((< (input-bar-line-position input) start))
    ((< (input-bar-line-position input) end)
     (setf (input-bar-line-position input) start))
    (t
     (decf (input-bar-line-position input) (- end start)))))

(defun input-bar-insert-char (input char)
  "Insert @var{char} into the input at the current
position. @var{input} must be of type @var{input-bar-line}. Input
functions are passed this structure as their first argument."
  (vector-push-extend #\_ (input-bar-line-string input))
  (let* ((pos (input-bar-line-position input))
         (pos-assoc (get-formatted-command pos (input-bar-line-output-assoc input)
                                           #'string-pos-assoc-output))
         (adjusted-pos (or (and pos-assoc (1- (second (string-pos-assoc-input pos-assoc))))
                           pos)))
    (dformat 0 "~a ~%" (input-bar-line-string input))
    (replace (input-bar-line-string input) (input-bar-line-string input)
             :start2 adjusted-pos :start1 (1+ adjusted-pos)))
  (dformat 0 "~a ~%" (input-bar-line-string input))
  (setf (char (input-bar-line-string input) (input-bar-line-position input)) char)
  (incf (input-bar-line-position input)))

(defun input-bar-substring (input start end)
  "Return a the substring in INPUT bounded by START and END."
  (subseq (input-bar-line-string input) start end))


;;; "interactive" input functions

(defun input-bar-find-completions (str completions)
  (if (or (functionp completions)
          (and (symbolp completions)
               (fboundp completions)))
      (funcall completions str)
      (remove-if-not (lambda (elt)
                       (when (listp elt)
                         (setf elt (car elt)))
                       (and (<= (length str) (length elt))
                            (string= str elt
                                     :end1 (length str)
                                     :end2 (length str))))
                     completions)))

(defun input-bar-complete (bar input direction)
  ;; reset the completion list if this is the first time they're
  ;; trying to complete.
  (unless (find (input-bar-last-command bar) '(input-bar-complete-forward
                                       input-bar-complete-backward))
    (setf (input-bar-current-completions bar) (input-bar-find-completions (input-bar-substring input 0 (input-bar-point input)) (input-bar-completions bar))
          (input-bar-current-completions-idx bar) -1))
  (if (input-bar-current-completions bar)
      (progn
        ;; Insert the next completion
        (input-bar-delete-region input 0 (input-bar-point input))
        (if (eq direction :forward)
            (progn
              (incf (input-bar-current-completions-idx bar))
              (when (>= (input-bar-current-completions-idx bar) (length (input-bar-current-completions bar)))
                (setf (input-bar-current-completions-idx bar) 0)))
            (progn
              (decf (input-bar-current-completions-idx bar))
              (when (< (input-bar-current-completions-idx bar) 0)
                (setf (input-bar-current-completions-idx bar) (1- (length (input-bar-current-completions bar)))))))
        (let ((elt (nth (input-bar-current-completions-idx bar) (input-bar-current-completions bar))))
          (input-bar-insert-string input (if (listp elt) (first elt) elt))
          (input-bar-insert-char input #\Space)))
      :error))

(defun input-bar-complete-forward (bar input key)
  (declare (ignore key))
  (input-bar-complete bar input :forward))

(defun input-bar-complete-backward (bar input key)
  (declare (ignore key))
  (input-bar-complete bar input :backward))

(defun input-bar-delete-backward-char (bar input key)
  (declare (ignore key) (ignore bar))
  (let ((pos (input-bar-line-position input)))
    (cond ((or (<= (length (input-bar-line-string input)) 0)
               (<= pos 0))
           :error)
          (t
             (replace (input-bar-line-string input) (input-bar-line-string input)
                      :start2 pos :start1 (1- pos))
           (decf (fill-pointer (input-bar-line-string input)))
           (decf (input-bar-line-position input))))))

(defun input-bar-delete-forward-char (bar input key)
  (declare (ignore key) (ignore bar))
  (let ((pos (input-bar-line-position input)))
    (cond ((>= pos
               (length (input-bar-line-string input)))
           :error)
          (t
           (replace (input-bar-line-string input) (input-bar-line-string input)
                    :start1 pos :start2 (1+ pos))
           (decf (fill-pointer (input-bar-line-string input)))))))

(defun input-bar-forward-kill-word (bar input key)
  (declare (ignore key) (ignore bar))
  (let* ((p1 (position-if 'alphanumericp (input-bar-line-string input) :start (input-bar-line-position input)))
         (p2 (and p1 (position-if-not 'alphanumericp (input-bar-line-string input) :start p1))))
    (input-bar-delete-region input (input-bar-point input) (or p2 (length (input-bar-line-string input))))))

(defun input-bar-backward-kill-word (bar input key)
  (declare (ignore key) (ignore bar))
  (let* ((p1 (position-if 'alphanumericp (input-bar-line-string input) :end (input-bar-line-position input) :from-end t))
         (p2 (and p1 (position-if-not 'alphanumericp (input-bar-line-string input) :end p1 :from-end t))))
    (input-bar-delete-region input (input-bar-point input) (or (and p2 (1+ p2)) 0))))

(defun input-bar-forward-word (bar input key)
  (declare (ignore key) (ignore bar))
  (let* ((p1 (position-if 'alphanumericp (input-bar-line-string input) :start (input-bar-line-position input)))
         (p2 (and p1 (position-if-not 'alphanumericp (input-bar-line-string input) :start p1))))
    (setf (input-bar-line-position input) (or p2 (length (input-bar-line-string input))))))

(defun input-bar-backward-word (bar input key)
  (declare (ignore key) (ignore bar))
  (let* ((p1 (position-if 'alphanumericp (input-bar-line-string input) :end (input-bar-line-position input) :from-end t))
         (p2 (and p1 (position-if-not 'alphanumericp (input-bar-line-string input) :end p1 :from-end t))))
    (setf (input-bar-line-position input) (or (and p2 (1+ p2))
                                          0))))

(defun input-bar-forward-char (bar input key)
  (declare (ignore key) (ignore bar))
  (let ((pos (translate-position-out input (input-bar-line-position input))))
    (setf (input-bar-line-position input) (translate-position-in input (1+ pos))))
  (when (> (input-bar-line-position input)
           (length (input-bar-line-string input)))
    (setf (input-bar-line-position input) (length (input-bar-line-string input)))))

(defun input-bar-backward-char (bar input key)
  (declare (ignore key) (ignore bar))
  (decf (input-bar-line-position input))
  (when (< (input-bar-line-position input) 0)
    (setf (input-bar-line-position input) 0)))

(defun input-bar-move-beginning-of-line (bar input key)
  (declare (ignore key) (ignore bar))
  (setf (input-bar-line-position input) 0))

(defun input-bar-move-end-of-line (bar input key)
  (declare (ignore key) (ignore bar))
  (setf (input-bar-line-position input) (length (input-bar-line-string input))))

(defun input-bar-kill-line (bar input key)
  (declare (ignore key) (ignore bar))
  (unless (= (input-bar-line-position input) (length (input-bar-line-string input)))
    (set-x-selection (subseq (input-bar-line-string input) (input-bar-line-position input))))
  (setf (fill-pointer (input-bar-line-string input)) (input-bar-line-position input)))

(defun input-bar-kill-to-beginning (bar input key)
  (declare (ignore key) (ignore bar))
  (unless (= (input-bar-line-position input) 0)
    (set-x-selection (subseq (input-bar-line-string input) 0 (input-bar-line-position input))))
  (replace (input-bar-line-string input) (input-bar-line-string input)
           :start2 (input-bar-line-position input) :start1 0)
  (decf (fill-pointer (input-bar-line-string input)) (input-bar-line-position input))
  (setf (input-bar-line-position input) 0))

;; TODO: Remove global state
(defun input-bar-history-back (bar input key)
  (declare (ignore key))
  (when (= (input-bar-line-history input) -1)
    (setf (input-bar-line-history-bk input) (input-bar-line-string input)))
  (incf (input-bar-line-history input))
  (if (>= (input-bar-line-history input)
          (length (input-bar-history bar)))
      (progn
        (decf (input-bar-line-history input))
        :error)
      (setf (input-bar-line-string input) (make-input-string (elt (input-bar-history bar) (input-bar-line-history input)))
            (input-bar-line-position input) (length (input-bar-line-string input)))))

(defun input-bar-history-forward (bar input key)
  (declare (ignore key))
  (decf (input-bar-line-history input))
  (cond ((< (input-bar-line-history input) -1)
         (incf (input-bar-line-history input))
         :error)
        ((= (input-bar-line-history input) -1)
         (setf (input-bar-line-string input) (input-bar-line-history-bk input)
               (input-bar-line-position input) (length (input-bar-line-string input))))
        (t
         (setf (input-bar-line-string input) (make-input-string (elt (input-bar-history bar) (input-bar-line-history input)))
               (input-bar-line-position input) (length (input-bar-line-string input))))))

(defun input-bar-self-insert (bar input key)
  (declare (ignore bar))
  (let ((char (xlib:keysym->character *display* (key-keysym key))))
    (if (or (key-mods-p key) (null char)
            (not (characterp char)))
        :error
        (input-bar-insert-char input char))))

(defun input-bar-yank-selection (bar input key)
  (declare (ignore key))
  ;; if we own the selection then just insert it.
  (if (getf *x-selection* :primary)
      (input-bar-insert-string input (getf *x-selection* :primary))
      (xlib:convert-selection :primary
                              :string (input-bar-window bar)
                              :stumpwm-selection)))

(defun input-bar-yank-clipboard (bar input key)
  (declare (ignore key))
  (if (getf *x-selection* :clipboard)
      (input-bar-insert-string input (getf *x-selection* :clipboard))
      (xlib:convert-selection :clipboard
                              :string (input-bar-window bar)
                              :stumpwm-selection)))

(defun input-bar-space-delimit (bar input key)
  (declare (ignore key) (ignore bar))
  (let* ((p1 (position-if-not (lambda (x) (string-equal x " ")) (input-bar-line-string input) :start (input-bar-line-position input)))
         (p2 (and p1 (position-if (lambda (x) (string-equal x " ")) (input-bar-line-string input) :start p1))))
    (setf (input-bar-line-position input) (or p2 (length (input-bar-line-string input))))
    (setf (input-bar-line-selection input)
          (input-bar-substring input p1 p2))
    (echo (input-bar-line-selection input))))

(defun input-bar-bracket-delimit (bar input key)
  (declare (ignore key) (ignore bar))
  (let* ((p1 (position-if (lambda (x) (string-equal x "[")) (input-bar-line-string input) :start (input-bar-line-position input)))
         (p2 (and p1 (position-if (lambda (x) (string-equal x "]")) (input-bar-line-string input) :start p1))))
    (setf (input-bar-line-position input) (or p2 (length (input-bar-line-string input))))
    (setf (input-bar-line-selection input)
          (input-bar-substring input (+ p1 1) p2))
    ;;(echo (input-bar-line-selection input))
    :done))

(defun create-coordinate-list (command-list)
  (reduce
   (lambda (lst item)
     (let* ((start-pos (if (equalp lst nil) 0 (+ (second (first (last lst))) 2))))
       (append lst
               (list (list start-pos
                           (+ start-pos (length (second item)) -1)
                           (first item))))))
   command-list
   :initial-value nil))

(defun input-bar-search-command (input pos)
  (declare (ignore input))
  (reduce (lambda (lst item)
            (if (and (>= pos (first item))
                     (<= pos (second item)))
                (third item)
                lst))
          (create-coordinate-list *default-command-list*)
          :initial-value nil))

;; TODO: Can the logic in this be cleaned up?
#|(defun input-bar-search-command (input pos)
  (unless (or (string-equal (input-bar-line-string input) "")
              (> pos (length (input-bar-line-string input))))
    (let* ((pos-assoc (get-formatted-command pos (input-bar-line-output-assoc input)
                                             #'string-pos-assoc-output))
           (cmd (and pos-assoc (string-pos-assoc-command pos-assoc))))
      (if (not (null cmd))
          cmd
          (let* ((p1 (position-if (lambda (x) (string-equal x "[")) (input-bar-line-string input) :end pos :from-end t))
                 (p2 (and p1 (position-if (lambda (x) (string-equal x "]")) (input-bar-line-string input) :start pos))))
            (if (and (numberp p1) (numberp p2))
                (input-bar-substring input (+ p1 1) p2)
                nil))))))|#


;;; Misc functions

(defun translate-position-in (input pos)
  "Translates cursor coordinates in an output string to proper coordinates in
   the corresponding input string."
  (loop with i = pos
        for pos-assoc in (input-bar-line-output-assoc input)
        do (let* ((formatted (string-pos-assoc-output pos-assoc))
                  (unformatted (string-pos-assoc-input pos-assoc))
                  (unformatted-diff (- (second unformatted) (first unformatted)))
                  (pos-adjust (if (> pos (second formatted))
                                  1
                                  (- (second formatted) pos)))
                  (input-adjust (if (= pos (second formatted))
                                    (- unformatted-diff 2)
                                    unformatted-diff)))
             (when (>= pos (first formatted))
               (setq i (+ i (- input-adjust pos-adjust)))))
        finally (return i)))

;; FIXME: This still doesn't really work.
(defun translate-position-out (input pos)
  "Translates cursor coordinates in an input string to proper coordinates in
   the corresponding output string."
  (loop with i = 0
        with prev-start = (length (input-bar-line-string input))
        for pos-assoc in (input-bar-line-output-assoc input)
        do (let* ((formatted (string-pos-assoc-output pos-assoc))
                  (unformatted (string-pos-assoc-input pos-assoc))
                  (formatted-diff (- (second formatted) (first formatted))))
             ;;(format t "f: ~a u: ~a f-d: ~a u-d: ~a    i: ~a ~%"
             ;;        formatted unformatted formatted-diff unformatted-diff i)
             (cond
               ((>= pos (second unformatted))
                (setq i (+ i (1+ formatted-diff) (- prev-start (second unformatted)))))
                ;;(setq i (+ i unformatted-diff)))
               ((and (> pos (first unformatted))
                     (< pos (second unformatted)))
                ;;(format t "Inside brackets~%")
                (cond
                  ((< pos (- (1- (second unformatted)) (1+ formatted-diff)))
                   ;;(format t "First: ~a~%" (- (1- (second unformatted)) (1+ formatted-diff)))
                   (setq i (+ i formatted-diff)))
                  ((< pos (second unformatted))
                   ;;(format t "Second: ~a Adding: ~a~%" (1- (second unformatted)) (- (- (second unformatted) 2) pos))
                   (setq i (+ i (- (- (second unformatted) 2) pos)))))))

             ;;(format t "prev-start: ~a current end: ~a ~%" prev-start (second unformatted))
             (setq prev-start (first unformatted)))
                   ;;(setq i (+ i (- (1- second unformatted) (1+ formatted-diff))))))))
        finally (return i)))

(defun get-formatted-command (pos table getter)
  "Checks if the command under the cursor has been formatted."
  (loop for pos-assoc in table
        do (let* ((range (funcall getter pos-assoc)))
             (when (and (>= pos (first range))
                        (<= pos (second range)))
               (return pos-assoc)))
        finally (return nil)))

(defun input-bar-process (bar prompt input code state)
  "Process the key (code and state), given the current input
buffer. Returns a new modified input buffer."
  ;;(dformat 0 "~a, ~a, ~a" input code state)
  (labels ((process-key (code state)
             "Call the appropriate function based on the key
pressed. Return 'done when the use has signalled the finish of his
input (pressing Return), nil otherwise."
             (let* ((key (code-state->key code state))
                    (command (and key (lookup-key (input-bar-key-map bar) key t))))
               (if command
                   (prog1
                       (funcall command bar input key)
                     (setf (input-bar-last-command bar) command))
                   :error))))
    (case (process-key code state)
      (:done
       (unless (or (input-bar-line-password input)
                   (and (input-bar-history-ignore-duplicates bar)
                        (string= (input-bar-line-string input) (first (input-bar-history bar)))))
         (push (input-bar-line-string input) (input-bar-history bar)))
       :done)
      (:abort
       (throw :abort t))
      (:error
       ;; FIXME draw inverted text
       (draw-input-bar-bucket bar prompt input "" t)
       nil)
      (t
       (draw-input-bar-bucket bar prompt input)
       nil))))

(defun start-bar (bar &optional code x y)
  "Read a command from the user. @var{initial-text} is optional. When
supplied, the text will appear in the prompt."
  (let ((cmd (completing-bar-read bar *default-command-prompt* (all-commands) :code code :x x :y y)))
    ;;(unless cmd
    ;;  (throw 'error :abort))
    (when (plusp (length cmd))
      (eval-command cmd t))))