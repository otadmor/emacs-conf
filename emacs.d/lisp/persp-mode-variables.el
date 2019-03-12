;; -*- lexical-binding: t; -*-
(require 'persp-mode)

;; COPIED FROM multiple-cursors-core.el
(setq persp-variables-has-minibuffer nil)

(defvar pmv/cursor-specific-vars nil
  "A list of vars that need to be tracked on a per-perspective basis.")

(push 'persp-variables-has-minibuffer pmv/cursor-specific-vars)

(defun* pmv/store-current-state-in-persp (&optional (persp (get-current-persp)))
  (dolist (var pmv/cursor-specific-vars)
    (when (boundp var) (set-persp-parameter var (symbol-value var) persp)))
  persp)

(defun* pmv/restore-state-from-persp (&optional (persp (get-current-persp)))
  (dolist (var pmv/cursor-specific-vars)
    (when (boundp var) (set var (persp-parameter var persp))))
  persp)

(defun persp-variables-before-deactivate-hook(frame-or-window)
  (setq persp-variables-has-minibuffer (not (eq (active-minibuffer-window) nil)))
  (let (
        (persp (pmv/store-current-state-in-persp))
        )
    (when persp-variables-has-minibuffer
      (select-window (ivy--get-window ivy-last))
      )
    persp))

(defmacro ivy-quit-and-run-keep-windows (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  (declare (indent 0))
  `(progn
     (run-at-time nil nil
                  (lambda ()
                    (let (
                          (new-window-configuration (current-window-configuration))
                          )
                      (ivy-quit-and-run
                        (set-window-configuration new-window-configuration)
                        ,@body))))))

(defun persp-variables-after-activate-hook(frame-or-window)
  (let (
        (previous-persp-has-minibuffer (and ivy-last persp-variables-has-minibuffer))
        )
    (let (
          (persp (pmv/restore-state-from-persp))
          )
      (let (
            (current-persp-has-minibuffer (and ivy-last persp-variables-has-minibuffer))
            )
        (cond
         ((and previous-persp-has-minibuffer current-persp-has-minibuffer)
          (ivy-quit-and-run-keep-windows (ivy-resume)))
         ((and (not previous-persp-has-minibuffer) current-persp-has-minibuffer)
          (run-at-time nil nil (lambda () (ivy-resume))))
         ((and previous-persp-has-minibuffer (not current-persp-has-minibuffer))
          (ivy-quit-and-run-keep-windows))))
    persp)))

(add-hook 'persp-before-deactivate-functions #'persp-variables-before-deactivate-hook)
(add-hook 'persp-activated-functions #'persp-variables-after-activate-hook)

(provide 'persp-mode-variables)
