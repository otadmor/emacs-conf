(require 'persp-mode)

;; COPIED FROM multiple-cursors-core.el

(defvar pmv/cursor-specific-vars '()
  "A list of vars that need to be tracked on a per-perspective basis.")

(defun* pmv/store-current-state-in-persp (&optional (persp (get-current-persp)))
  (dolist (var pmv/cursor-specific-vars)
    (when (boundp var) (set-persp-parameter var (symbol-value var) persp)))
  persp)

(defun* pmv/restore-state-from-persp (&optional (persp (get-current-persp)))
  (dolist (var pmv/cursor-specific-vars)
    (when (boundp var) (set var (persp-parameter var persp))))
  persp)

(defun persp-variables-before-deactivate-hook(frame-or-window)
  (pmv/store-current-state-in-persp))

(defun persp-variables-after-activate-hook(frame-or-window)
  (pmv/restore-state-from-persp))

(add-hook 'persp-before-deactivate-functions #'persp-variables-before-deactivate-hook)
(add-hook 'persp-activated-functions #'persp-variables-after-activate-hook)

(provide 'persp-mode-variables)
