; (defface sync-window-face ;; originally copied from font-lock-function-name-face
;   '((((class color) (min-colors 88) (background light)) (:foreground "Yellow" :background "Blue1"))
;     (((class color) (min-colors 88) (background dark)) (:foreground "Red" :background  "LightSkyBlue"))
;     (((class color) (min-colors 16) (background light)) (:foreground "Blue" :background "Yellow"))
;     (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue" :background "Yellow"))
;     (((class color) (min-colors 8)) (:foreground "blue" :bold t))
;     (t (:bold t)))
;   "Face used to highlight regions in `sync-window-mode' slaves."
;   :group 'sync-window)

; (defvar sync-window-overlay nil
;   "Overlay for current master region in `sync-window-mode' slaves.")
; (make-variable-buffer-local 'sync-window-overlay)

; (defun sync-window-cleanup ()
;   "Clean up after `sync-window-mode'."
;   (interactive)
;   (if (overlayp sync-window-overlay)
;       (progn
;     (delete-overlay sync-window-overlay)
;     (setq sync-window-overlay nil))
;     (remove-overlays (point-min) (point-max) 'sync-window-slave t)))

; (defvar sync-window-master-hook nil
;   "Hooks to be run by `sync-window' in the master window ")

; (defun sync-window (&optional display-start)
;   "Synchronize point position other window in current frame.
; Only works if there are exactly two windows in the active wrame not counting the minibuffer."
;   (interactive)
;   (when (= (count-windows 'noMiniBuf) 2)
;     (let ((p (line-number-at-pos))
;       (start (line-number-at-pos (or display-start (window-start))))
;       (vscroll (window-vscroll))
;       breg ereg)
;       (when (use-region-p)
;     (setq breg (line-number-at-pos (region-beginning))
;           ereg  (line-number-at-pos (if (looking-back "\n") (1- (region-end)) (region-end)))))
;       (run-hooks 'sync-window-master-hook)
;       (other-window 1)
;       (goto-char (point-min))
;       (when breg
;     (sync-window-cleanup)
;     (overlay-put (setq sync-window-overlay (make-overlay (line-beginning-position breg) (line-end-position ereg))) 'face 'sync-window-face)
;     (overlay-put sync-window-overlay 'sync-window-slave t))
;       (setq start (line-beginning-position start))
;       (forward-line (1- p))
;       (set-window-start (selected-window) start)
;       (set-window-vscroll (selected-window) vscroll)
;       (other-window 1)
;       (unless display-start
;     (redisplay t))
;       )))

; (defvar sync-window-mode-hook nil
;   "Hooks to be run at start of `sync-window-mode'.")

; (define-minor-mode sync-window-mode
;   "Synchronized view of two buffers in two side-by-side windows."
;   :group 'windows
;   :lighter " ⇕"
;   (if sync-window-mode
;       (progn
;     (add-hook 'post-command-hook 'sync-window-wrapper 'append t)
;     (add-to-list 'window-scroll-functions 'sync-window-wrapper)
;     (run-hooks 'sync-window-mode-hook)
;     (sync-window))
;     (remove-hook 'post-command-hook 'sync-window-wrapper t)
;     (setq window-scroll-functions (remove 'sync-window-wrapper window-scroll-functions))
;     ))

; (defun sync-window-wrapper (&optional window display-start)
;   "This wrapper makes sure that `sync-window' is fired from `post-command-hook'
; only when the buffer of the active window is in `sync-window-mode'."
;   (with-selected-window (or window (selected-window))
;     (when sync-window-mode
;       (sync-window display-start))))

; (provide 'sync-window)


(defun sync-window (&optional display-start)
  "Synchronize point position other window in current frame.
Only works if there are exactly two windows in the active wrame not counting the minibuffer."
  (interactive)
  (when (= (count-windows 'noMiniBuf) 2)
    (let ((p (point))
      (start (or display-start (window-start)))
      (vscroll (window-vscroll)))
      (other-window 1)
      (goto-char (min (max p (point-min)) (point-max)))
      (set-window-start (selected-window) start)
      (set-window-vscroll (selected-window) vscroll)
      (other-window 1)
      )))

(define-minor-mode sync-window-mode
  "Synchronized view of two buffers in two side-by-side windows."
  :group 'windows
  :lighter " ⇕"
  (if sync-window-mode
      (progn
    (add-hook 'post-command-hook 'sync-window-wrapper 'append t)
    (add-to-list 'window-scroll-functions 'sync-window-wrapper)
    (sync-window))
    (remove-hook 'post-command-hook 'sync-window-wrapper t)
    (setq window-scroll-functions (remove 'sync-window-wrapper window-scroll-functions))
    ))

(defun sync-window-wrapper (&optional window display-start)
  "This wrapper makes sure that `sync-window' is fired from `post-command-hook'
only when the buffer of the active window is in `sync-window-mode'."
  (with-selected-window (or window (selected-window))
    (when sync-window-mode
      (sync-window display-start))))

(provide 'sync-window)
