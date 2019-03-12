;; -*- lexical-binding: t; -*-
(require 'persp-mode)

;; COPIED FROM multiple-cursors-core.el
(setq persp-variables-has-minibuffer nil)

(defvar pmv/cursor-specific-vars nil
  "A list of vars that need to be tracked on a per-perspective basis.")

(push 'persp-variables-has-minibuffer pmv/cursor-specific-vars)



; (defun* pmv/store-current-state-in-persp (&optional (persp (get-current-persp)))
;   (dolist (var pmv/cursor-specific-vars)
;     (if (boundp var)
;         (set-persp-parameter var (symbol-value var) persp)
;       (delete-persp-parameter var persp)
;       ))
;   persp)

; (defun* persp-has-parameter
;     (param-name &optional value (persp (get-current-persp)))
;   (let* ((params (safe-persp-parameters persp))
;          (old-cons (assq param-name params)))
;     (not (null old-cons))))

; (defun* pmv/restore-state-from-persp (&optional (persp (get-current-persp)))
;   (dolist (var pmv/cursor-specific-vars)
;     (if (persp-has-parameter var persp)
;         (set var (persp-parameter var persp))
;       (makunbound var)))
;   persp)

; (defun ivy-state-collection(x) )

(defun* pmv/store-current-state-in-persp (&optional (persp (get-current-persp)))
  (dolist (var pmv/cursor-specific-vars)
    (when (boundp var) (set-persp-parameter var (symbol-value var) persp)))
  persp)

(defun* pmv/restore-state-from-persp (&optional (persp (get-current-persp)))
  (dolist (var pmv/cursor-specific-vars)
    (set var (persp-parameter var persp)))
  persp)

; (defun ivy-cancel-timers()
;   (when ivy--pulse-timer
;     (cancel-timer ivy--pulse-timer)
;     (setq ivy--pulse-timer nil)
;     (ivy--pulse-cleanup))
;   (when (timerp ivy-occur-timer)
;     (cancel-timer ivy-occur-timer)
;     (setq ivy-occur-timer nil)
;     (swiper--cleanup))
;   (when ivy--exhibit-timer
;     (cancel-timer ivy--exhibit-timer)
;     (setq ivy--exhibit-timer nil)
;     (ivy--exhibit)))

(defun persp-variables-before-deactivate-hook(frame-or-window)
  ; (ivy-cancel-timers)
  (setq persp-variables-has-minibuffer (not (eq (active-minibuffer-window) nil)))
  ; (setq persp-variables-has-minibuffer-focus ???)
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
     ; (ivy-cancel-timers)
     (run-at-time nil nil
                  (lambda ()
                    (let (
                          (new-window-configuration (current-window-configuration))
                          )
                      (select-window (active-minibuffer-window))
                      ; (message "after select window")
                      ; (ivy-cancel-timers)
                      (ivy-quit-and-run
                        ; (message "before set conf")
                        (set-window-configuration new-window-configuration)
                        ; (message "after set cond")
                        ,@body))))))


(defun ivy--maybe-null(orig-fun &rest args)
  (when ivy-last
    (apply orig-fun args)))
(advice-add 'ivy--exhibit :around #'ivy--maybe-null)
; (advice-add 'ivy--queue-exhibit :around #'ivy--maybe-null)
; (advice-add 'ivy-state-dynamic-collection :around #'ivy--maybe-null)


; (defun ivy-first-maybe-null(orig-fun &rest args)
;   (when (car args)
;     (apply orig-fun args)))
; (advice-add 'ivy-state-dynamic-collection :around #'ivy-first-maybe-null)


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
        ; (ivy-cancel-timers)
        (cond
         ((and previous-persp-has-minibuffer current-persp-has-minibuffer)
          (ivy-quit-and-run-keep-windows (ivy-resume)))
         ((and (not previous-persp-has-minibuffer) current-persp-has-minibuffer)
          (run-at-time nil nil (lambda () (ivy-resume))))
         ((and previous-persp-has-minibuffer (not current-persp-has-minibuffer))
                                        ; (remove-hook 'post-command-hook #'ivy--queue-exhibit)
          (ivy-quit-and-run-keep-windows))))
      ; (message "done cond")
      (ivy--exhibit)
    persp)))

(add-hook 'persp-before-deactivate-functions #'persp-variables-before-deactivate-hook)
(add-hook 'persp-activated-functions #'persp-variables-after-activate-hook)

(provide 'persp-mode-variables)
