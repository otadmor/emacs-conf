;; -*- lexical-binding: t; -*-
(require 'persp-mode)

;; COPIED FROM multiple-cursors-core.el
(setq persp-variables-has-minibuffer nil)
(setq persp-variables-has-minibuffer-focus nil)

(defvar pmv/cursor-specific-vars nil
  "A list of vars that need to be tracked on a per-perspective basis.")

(push 'persp-variables-has-minibuffer pmv/cursor-specific-vars)
(push 'persp-variables-has-minibuffer-focus pmv/cursor-specific-vars)



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

(defun ivy-cancel-timers()
  (when ivy--pulse-timer
    (cancel-timer ivy--pulse-timer)
    (setq ivy--pulse-timer nil)
    (ivy--pulse-cleanup))
  (when (timerp ivy-occur-timer)
    (cancel-timer ivy-occur-timer)
    (setq ivy-occur-timer nil)
    (swiper--cleanup))
  (when ivy--exhibit-timer
    (cancel-timer ivy--exhibit-timer)
    (setq ivy--exhibit-timer nil)
    (ivy--exhibit)))

(defmacro ivy-quit-and-run-keep-windows (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  (let (
        (conf (gensym "window-conf"))
        )
    `(progn
       (run-at-time nil nil
                    (lambda ()
                      (let (
                            (,conf (current-window-configuration))
                            )
                        (ivy-quit-and-run
                          (set-window-configuration ,conf)
                          ,@body)))))))

(defmacro ivy-resume-keep-frame-selected-window (&rest body)
  (let (
        (win (gensym "selected-win"))
        )
    `(progn
       (run-at-time nil nil
                    (lambda ()
                      (let (
                            (,win (frame-selected-window))
                            )
                        (run-at-time nil nil
                                     (lambda ()
                                       (select-window ,win)
                                       ,@body))
                        (ivy-resume)))))))

(defun persp-variables-before-deactivate-hook(frame-or-window)
  (ivy-cancel-timers)
  (setq persp-variables-has-minibuffer (not (null (active-minibuffer-window))))
  (setq persp-variables-has-minibuffer-focus (eq (active-minibuffer-window) (frame-selected-window)))
  (when (and persp-variables-has-minibuffer persp-variables-has-minibuffer-focus)
    (select-window (ivy--get-window ivy-last)))
  (when (and ivy-last persp-variables-has-minibuffer)
    (ivy-quit-and-run-keep-windows)) ; quit must run async.
  (pmv/store-current-state-in-persp))

(defun persp-variables-after-activate-hook(frame-or-window)
  (run-at-time nil nil
    (lambda ()
      (ivy-cancel-timers)
      (pmv/restore-state-from-persp) ; should run after killing the minibuffer on deactivate.
      (when (and ivy-last persp-variables-has-minibuffer)
        (ivy-resume-keep-frame-selected-window
         (when persp-variables-has-minibuffer-focus
           (select-window (active-minibuffer-window))))))))

(add-hook 'persp-before-deactivate-functions #'persp-variables-before-deactivate-hook)
(add-hook 'persp-activated-functions #'persp-variables-after-activate-hook)


(provide 'persp-mode-variables)
