
                                        ; lockstep.el
; Trevor Jim
; Keep frames in lock step, useful for pair programming with emacsclients
; Requires Emacs 24

;; (require 'cl)

(add-hook 'post-command-hook #'lockstep-point)
(add-hook 'window-configuration-change-hook #'lockstep-frame)

(defvar lockstep-frames nil)

(defun lockstep ()
  "Synchronize this frame's windows and points."
  (interactive)
  (turn-on-lockstep))

(defun turn-on-lockstep ()
  "Synchronize this frame's windows and points."
  (interactive)
  (when lockstep-show-cursor-on-all-frames
    (setq cursor-type nil))
  (unless lockstep-frames
    (lockstep-load))
  (when (not (memq (selected-frame) lockstep-frames))
    (let ((a-lockstep-frame (when lockstep-frames (car lockstep-frames))))
      (push (selected-frame) lockstep-frames)
      (lockstep-frame a-lockstep-frame))))

(defun turn-off-lockstep ()
  "Turn off synchronization for this frame."
  (interactive)
  (setq lockstep-frames (remq (selected-frame) lockstep-frames)))

(setq lockstep--recursion-protect t)
(defun lockstep-needed ()
  (setq lockstep-frames (cl-remove-if-not 'frame-live-p lockstep-frames))
  ;; (setq lockstep-frames (cl-remove-if-not (lambda (frame) (eq (framep frame) 'x)) lockstep-frames))
  (and lockstep--recursion-protect
       (> (length lockstep-frames) 1)
       (memq (selected-frame) lockstep-frames)))

(defun lockstep-frame (&optional master-frame)
  "Synchronize window configurations of frames."
  (when (lockstep-needed)
    (remove-hook 'window-configuration-change-hook 'lockstep-frame)
    (unwind-protect
        (let* ((this-frame (or master-frame (selected-frame)))
               (other-frames (remove-if (lambda (frame) (equal this-frame frame)) lockstep-frames)))
          (cl-loop for other-frame in other-frames
                do (let ((this-frame-windows
                          (cl-remove-if-not 'window-live-p
                                         (remove-if 'window-minibuffer-p (window-list this-frame))))
                         (other-frame-windows
                          (cl-remove-if-not 'window-live-p
                                         (remove-if 'window-minibuffer-p (window-list other-frame)))))

                                        ; force this-frame and other-frame to have the same number of windows
                     (while (not (equal (length this-frame-windows) (length other-frame-windows)))
                       (when (< (length this-frame-windows) (length other-frame-windows))
                         (delete-window (pop other-frame-windows)))
                       (when (> (length this-frame-windows) (length other-frame-windows))
                         (split-window (car other-frame-windows))
                         (setq other-frame-windows (remove-if 'window-minibuffer-p (window-list other-frame)))))
                     ;; force this-frame-windows and other-frame-windows to have same configurations
                     (while this-frame-windows
                       (let ((this-frame-window (pop this-frame-windows))
                             (other-frame-window (pop other-frame-windows)))
                         (lockstep-window-state-put (window-state-get this-frame-window) other-frame-window t)
                         (when master-frame
                           ;; if called from turn-on-lockstep, synchronize point as well
                           (set-window-start other-frame-window (window-start this-frame-window))
                           (set-window-point other-frame-window (window-point this-frame-window))))))))
      (add-hook 'window-configuration-change-hook 'lockstep-frame))))

; modified from emacs 24.4
(defun lockstep-window-state-put (state &optional window ignore)
  "Put window state STATE into WINDOW.
STATE should be the state of a window returned by an earlier
invocation of `window-state-get'.  Optional argument WINDOW must
specify a valid window and defaults to the selected one.  If
WINDOW is not live, replace WINDOW by a live one before putting
STATE into it.

Optional argument IGNORE non-nil means ignore minimum window
sizes and fixed size restrictions.  IGNORE equal `safe' means
windows can get as small as `window-safe-min-height' and
`window-safe-min-width'."
  (setq window-state-put-stale-windows nil)
  (setq window (window-normalize-window window))

  ;; When WINDOW is internal, reduce it to a live one to put STATE into,
  ;; see Bug#16793.
  (unless (window-live-p window)
    (let ((root (frame-root-window window)))
      (if (eq window root)
                  (setq window (frame-first-window root))
                (setq root window)
                (setq window (catch 'live
                                       (walk-window-subtree
                                                (lambda (window)
                                                  (when (window-live-p window)
                                                    (throw 'live window)))
                                                root))))
      (delete-other-windows-internal window root)))

  (let* ((frame (window-frame window))
                (head (car state))
                ;; We check here (1) whether the total sizes of root window of
                ;; STATE and that of WINDOW are equal so we can avoid
                ;; calculating new sizes, and (2) if we do have to resize
                ;; whether we can do so without violating size restrictions.
                (pixelwise (and (cdr (assq 'pixel-width state))
                                                (cdr (assq 'pixel-height state))))
                (totals (or (and pixelwise
                                                  (= (window-pixel-width window)
                                                     (cdr (assq 'pixel-width state)))
                                                  (= (window-pixel-height window)
                                                     (cdr (assq 'pixel-height state))))
                                     (and (= (window-total-width window)
                                                     (cdr (assq 'total-width state)))
                                                  (= (window-total-height window)
                                                     (cdr (assq 'total-height state))))))
                (min-height (cdr (assq
                                                   (if pixelwise 'min-pixel-height 'min-height)
                                                   head)))
                (min-width (cdr (assq
                                                  (if pixelwise 'min-pixel-width 'min-weight)
                                                  head))))
    (if (and nil
             (not totals)
                     (or (> min-height (window-size window nil pixelwise))
                                (> min-width (window-size window t pixelwise)))
                     (or (not ignore)
                                (and (setq min-height
                                                    (cdr (assq
                                                                  (if pixelwise
                                                                      'min-pixel-height-ignore
                                                                    'min-height-ignore)
                                                                  head)))
                                      (setq min-width
                                                    (cdr (assq
                                                                  (if pixelwise
                                                                      'min-pixel-width-ignore
                                                                    'min-width-ignore)
                                                                  head)))
                                      (or (> min-height
                                                     (window-size window nil pixelwise))
                                                  (> min-width
                                                     (window-size window t pixelwise)))
                                      (or (not (eq ignore 'safe))
                                                  (and (setq min-height
                                                                     (cdr (assq
                                                                                   (if pixelwise
                                                                                       'min-pixel-height-safe
                                                                                     'min-height-safe)
                                                                                   head)))
                                                       (setq min-width
                                                                     (cdr (assq
                                                                                   (if pixelwise
                                                                                       'min-pixel-width-safe
                                                                                     'min-width-safe)
                                                                                   head)))
                                                       (or (> min-height
                                                                      (window-size window nil pixelwise))
                                                                   (> min-width
                                                                      (window-size window t pixelwise))))))))
                ;; The check above might not catch all errors due to rounding
                ;; issues - so IGNORE equal 'safe might not always produce the
                ;; minimum possible state.  But such configurations hardly make
                ;; sense anyway.
                (error "Window %s too small to accommodate state" window)
      (setq state (cdr state))
      (setq window-state-put-list nil)
      ;; Work on the windows of a temporary buffer to make sure that
      ;; splitting proceeds regardless of any buffer local values of
      ;; `window-size-fixed'.  Release that buffer after the buffers of
      ;; all live windows have been set by `window--state-put-2'.
      (with-temp-buffer
                (set-window-buffer window (current-buffer))
                (window--state-put-1 state window nil totals pixelwise)
                (window--state-put-2 ignore pixelwise))
      (while window-state-put-stale-windows
                (let ((window (pop window-state-put-stale-windows)))
                  (when (eq (window-deletable-p window) t)
                    (delete-window window))))
      (window--check frame))))


(setq lockstep--windows-backup nil)
(defun lockstep-store ()
  (interactive)
  (let ((windows-list))
    (dolist (master-frame-window
             (cl-remove-if-not 'window-live-p (remove-if 'window-minibuffer-p (window-list (car lockstep-frames))))
             windows-list)
      (push (list
             (window-state-get master-frame-window)
             (window-start master-frame-window)
             (window-point master-frame-window)) windows-list))
    (setq lockstep--windows-backup windows-list)))

(defun lockstep-load ()
  (interactive)
  (let* ((other-frame (selected-frame))
         (other-frame-windows
          (cl-remove-if-not 'window-live-p (remove-if 'window-minibuffer-p (window-list other-frame)))))
    (while (not (equal (length lockstep--windows-backup) (length other-frame-windows)))
      (when (< (length lockstep--windows-backup) (length other-frame-windows))
        (delete-window (pop other-frame-windows)))
      (when (> (length lockstep--windows-backup) (length other-frame-windows))
        (split-window (car other-frame-windows))
        (setq other-frame-windows (remove-if 'window-minibuffer-p (window-list other-frame)))))
    ;; force this-frame-windows and other-frame-windows to have same configurations
    (while lockstep--windows-backup
      (let* ((this-frame-window (pop lockstep--windows-backup))
             (this-window-state (car this-frame-window))
             (this-window-start (cadr this-frame-window))
             (this-window-point (caddr this-frame-window))
             (other-frame-window (pop other-frame-windows)))
        (lockstep-window-state-put this-window-state other-frame-window t)
        ;; if called from turn-on-lockstep, synchronize point as well
        (set-window-start other-frame-window this-window-start)
        (set-window-point other-frame-window this-window-point)))))

(with-eval-after-load 'persp-mode

  (cl-defun lockstep-persp-before-switch (frame-or-window)
    (when (and (lockstep-needed)
               (eq frame-or-window 'frame))
      (let* (
             (this-frame (selected-frame))
             (other-frames (remove-if
                            (lambda (frame) (equal this-frame frame))
                            lockstep-frames))
             (persp (get-current-persp this-frame))
             (lockstep--recursion-protect nil)
             )
        (persp-frame-save-state this-frame)
        (cl-loop for frame in other-frames
              do (progn
                   (with-selected-frame frame
                     (setq persp-last-persp-name (safe-persp-name persp))
                     (set-frame-persp persp frame)
                     (persp-restore-window-conf frame persp)))))))
  (add-hook 'persp-activated-functions 'lockstep-persp-before-switch)

  (defun lockstep-frame-use-persp (&optional frame)
    (when (and (lockstep-needed)
               (not (eq this-command 'persp-switch)))
      (unwind-protect
          (let* (
                 (this-frame (or frame (selected-frame)))
                 (other-frames (remove-if
                                (lambda (frame) (equal this-frame frame))
                                lockstep-frames))
                 (persp (get-current-persp this-frame))
                 (lockstep--recursion-protect nil)
                 )
            (persp-frame-save-state this-frame)
            (cl-loop for frame in other-frames
                  do (progn
                       (with-selected-frame frame
                         (persp-restore-window-conf frame persp))))))))
  (defalias 'lockstep-frame 'lockstep-frame-use-persp)

  (defun lockstep-store-use-persp (&optional frame)
    (when (and (= (length lockstep-frames) 1)
               (not (eq this-command 'persp-switch)))
      (let* (
             (this-frame (or frame (selected-frame)))
             (lockstep--recursion-protect nil)
             (persp (get-current-persp this-frame))
             )
        (persp-frame-save-state this-frame))))
  (defalias 'lockstep-store 'lockstep-store-use-persp)

  (defun lockstep-load-use-persp (&optional frame)
    (when (and (= (length lockstep-frames) 0)
               (not (eq this-command 'persp-switch)))
      (let* (
             (this-frame (or frame (selected-frame)))
             (lockstep--recursion-protect nil)
             (persp (get-current-persp this-frame))
             )
        (if (null (safe-persp-window-conf (get-frame-persp this-frame)))
            (switch-to-buffer "*scratch*" nil t)
          (persp-restore-window-conf this-frame)))))
  (defalias 'lockstep-load 'lockstep-load-use-persp))


;; a fix for having lockstep with emacs server.
(defun unrecord-window-buffer-line-only (orig-fun &optional window buffer)
  (when (window-live-p window)
    (funcall orig-fun window buffer)))
(advice-add 'unrecord-window-buffer :around #'unrecord-window-buffer-line-only)


(defun lockstep--fake-cursor-p (o)
  "Predicate to check if an overlay is a fake cursor"
  (eq (overlay-get o 'type) 'lockstep--fake-cursor))

(defun lockstep--all-fake-cursors (&optional start end)
  (cl-remove-if-not 'lockstep--fake-cursor-p
                    (overlays-in (or start (point-min))
                                 (or end   (point-max)))))

(defmacro lockstep--for-each-fake-cursor (&rest forms)
  `(mapc #'(lambda (cursor) ,@forms)
         (lockstep--all-fake-cursors)))

(defun lockstep--remove-fake-cursors ()
  (lockstep--for-each-fake-cursor
   (delete-overlay cursor)))

;; (defface lockstep--cursor-face
;;   '((t (:inverse-video t)))
;;   "The face used for fake cursors"
;;   :group 'lockstep)

(defface lockstep--cursor-face
  '((t :inherit highlight))
  "The face used for fake cursors"
  :group 'lockstep)

(defface lockstep--region-face
  '((t :inherit region))
  "The face used for fake regions"
  :group 'lockstep)
(setq lockstep-show-cursor-on-all-frames nil)
(defun lockstep--create-fake-cursor-and-region (is-eol mark point)
  (when lockstep-show-cursor-on-all-frames
    (let ((overlay (make-overlay point (+ point (if is-eol 0 1)) nil t nil)))
      (overlay-put overlay 'window t)
      (overlay-put overlay 'priority -1000)
      (if is-eol
          (overlay-put overlay 'after-string (propertize " " 'face 'lockstep--cursor-face))
        (overlay-put overlay 'face 'lockstep--cursor-face))
      (unless is-eol
        (overlay-put overlay 'face 'lockstep--cursor-face))
      (overlay-put overlay 'type 'lockstep--fake-cursor)))
  (unless (null mark)
    (let ((overlay (if lockstep-show-cursor-on-all-frames
                       (if (< mark point)
                           (make-overlay mark point nil nil t)
                         (make-overlay (+ point 1) point nil nil t))
                     (make-overlay mark point nil nil t))))
      ;; (overlay-put overlay 'after-string
      ;; (propertize " " 'face 'mc/cursor-face))
      (overlay-put overlay 'window t)
      (overlay-put overlay 'face 'lockstep--region-face)
      (overlay-put overlay 'type 'lockstep--fake-cursor)))
  nil)

(defun lockstep-point ()
  "Synchronize point in all windows in other lockstep frames visiting this buffer."
  (when (lockstep-needed)
    (let* ((this-frame (selected-frame))
           (other-frames (remove-if (lambda (frame) (equal this-frame frame)) lockstep-frames)))
      (cl-loop for window in
            (cl-remove-if-not (lambda (window)
                             (and (window-live-p window)
                                  (equal (window-buffer window) (current-buffer))))
                           (cl-loop for frame in other-frames nconcing (window-list frame)))
            do (unless (eq window (selected-window))
                 (let (
                       (current-point (point))
                       (current-mark (and (region-active-p) (mark t)))
                       (is-eol (eolp))
                       )
                   (unless (eq window (selected-window))
                     (set-window-start window (window-start))
                     (set-window-point window current-point)
                     (with-selected-window window
                       (lockstep--remove-fake-cursors)
                       (lockstep--create-fake-cursor-and-region is-eol current-mark current-point))
                     (set-frame-selected-window (window-frame window) window))))))))

(defun lockstep-popup ()
  "Modify the popup library so that popups in a buffer are shown in all windows showing the buffer.

Invoke this if you want this behavior; it is useful for auto-complete.

It would be better to only show the popup in a window synchronized in another frame, but that does
not seem possible in Emacs 24."
  (interactive)
  (cl-defun popup-create (point
                          width
                          height
                          &key
                          min-height
                          around
                          (face 'popup-face)
                          mouse-face
                          (selection-face face)
                          (summary-face 'popup-summary-face)
                          scroll-bar
                          margin-left
                          margin-right
                          symbol
                          parent
                          parent-offset
                          keymap)
    "Create a popup instance at POINT with WIDTH and HEIGHT.

MIN-HEIGHT is a minimal height of the popup. The default value is
0.

If AROUND is non-nil, the popup will be displayed around the
point but not at the point.

FACE is a background face of the popup. The default value is POPUP-FACE.

SELECTION-FACE is a foreground (selection) face of the popup The
default value is POPUP-FACE.

If SCROLL-BAR is non-nil, the popup will have a scroll bar at the
right.

If MARGIN-LEFT is non-nil, the popup will have a margin at the
left.

If MARGIN-RIGHT is non-nil, the popup will have a margin at the
right.

SYMBOL is a single character which indicates a kind of the item.

PARENT is a parent popup instance. If PARENT is omitted, the
popup will be a root instance.

PARENT-OFFSET is a row offset from the parent popup.

KEYMAP is a keymap that will be put on the popup contents."
    (or margin-left (setq margin-left 0))
    (or margin-right (setq margin-right 0))
    (unless point
      (setq point
            (if parent (popup-child-point parent parent-offset) (point))))

    (save-excursion
      (goto-char point)
      (let* ((row (line-number-at-pos))
             (column (popup-current-physical-column))
             (overlays (make-vector height nil))
             (popup-width (+ width
                             (if scroll-bar 1 0)
                             margin-left
                             margin-right
                             (if symbol 2 0)))
             margin-left-cancel
             (window (selected-window))
             (window-start (window-start))
             (window-hscroll (window-hscroll))
             (window-width (window-width))
             (right (+ column popup-width))
             (overflow (and (> right window-width)
                            (>= right popup-width)))
             (foldable (and (null parent)
                            (>= column popup-width)))
             (direction (or
                         ;; Currently the direction of cascade popup won't be changed
                         (and parent (popup-direction parent))

                         ;; Calculate direction
                         (popup-calculate-direction height row)))
             (depth (if parent (1+ (popup-depth parent)) 0))
             (newlines (max 0 (+ (- height (count-lines point (point-max))) (if around 1 0))))
             current-column)
        ;; Case: no newlines at the end of the buffer
        (when (> newlines 0)
          (popup-save-buffer-state
            (goto-char (point-max))
            (insert (make-string newlines ?\n))))

        ;; Case: the popup overflows
        (if overflow
            (if foldable
                (progn
                  (cl-decf column (- popup-width margin-left margin-right))
                  (unless around (move-to-column column)))
              (when (not truncate-lines)
                ;; Truncate.
                (let ((d (1+ (- popup-width (- window-width column)))))
                  (cl-decf popup-width d)
                  (cl-decf width d)))
              (cl-decf column margin-left))
          (cl-decf column margin-left))

        ;; Case: no space at the left
        (when (and (null parent)
                   (< column 0))
          ;; Cancel margin left
          (setq column 0)
          (cl-decf popup-width margin-left)
          (setq margin-left-cancel t))

        (dotimes (i height)
          (let (overlay begin w (dangle t) (prefix "") (postfix ""))
            (when around
              (popup-vertical-motion column direction))
            (setq around t
                  current-column (popup-current-physical-column))

            (when (> current-column column)
              (backward-char)
              (setq current-column (popup-current-physical-column)))
            (when (< current-column column)
              ;; Extend short buffer lines by popup prefix (line of spaces)
              (setq prefix (make-string
                            (+ (if (= current-column 0)
                                   (- window-hscroll (current-column))
                                 0)
                               (- column current-column))
                            ? )))

            (setq begin (point))
            (setq w (+ popup-width (length prefix)))
            (while (and (not (eolp)) (> w 0))
              (setq dangle nil)
              (cl-decf w (char-width (char-after)))
              (forward-char))
            (if (< w 0)
                (setq postfix (make-string (- w) ? )))

            (setq overlay (make-overlay begin (point)))
;            (overlay-put overlay 'window window)
            (overlay-put overlay 'dangle dangle)
            (overlay-put overlay 'prefix prefix)
            (overlay-put overlay 'postfix postfix)
            (overlay-put overlay 'width width)
            (aset overlays
                  (if (> direction 0) i (- height i 1))
                  overlay)))
        (cl-loop for p from (- 10000 (* depth 1000))
              for overlay in (nreverse (append overlays nil))
              do (overlay-put overlay 'priority p))
        (let ((it (make-popup :point point
                              :row row
                              :column column
                              :width width
                              :height height
                              :min-height min-height
                              :direction direction
                              :parent parent
                              :depth depth
                              :face face
                              :mouse-face mouse-face
                              :selection-face selection-face
                              :summary-face summary-face
                              :margin-left margin-left
                              :margin-right margin-right
                              :margin-left-cancel margin-left-cancel
                              :scroll-bar scroll-bar
                              :symbol symbol
                              :cursor 0
                              :offset 0
                              :scroll-top 0
                              :current-height 0
                              :list nil
                              :newlines newlines
                              :overlays overlays
                              :keymap keymap)))
          (push it popup-instances)
          it))))
)

(provide 'lockstep)
