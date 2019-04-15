;; -*- lexical-binding: t; -*-
(require 'persp-mode)

;; COPIED FROM multiple-cursors-core.el
(setq persp-variables-has-minibuffer nil)
(setq persp-variables-has-minibuffer-focus nil)

(defvar pmv/cursor-specific-vars nil
  "A list of vars that need to be tracked on a per-perspective basis.")

(defvar pmv/cursor-clear-vars nil
  "A list of vars that will be set to nil when switching back to a perspective.")

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
  (dolist (var pmv/cursor-clear-vars)
    (set var nil))
  persp)

(defun ivy-cancel-timers()
  (when (not (null swiper--async-timer))
    (cancel-timer swiper--async-timer)
    (setq swiper--async-timer nil))
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
                        ;; ivy-text is reset by ivy--reset-state which called by
                        ;; ivy-resume. this forces re-calculations of
                        ;; ivy candidates when using dynamic collection.
                        ;; each dynamic collection should check
                        ;; ivy-text--persp-variables to see if ew resuming.
                        (setq ivy-text--persp-variables ivy-text)
                        (ivy-resume)))))))

(defun persp-variables-before-deactivate-hook(frame-or-window)
  (ivy-cancel-timers)
  (setq persp-variables-has-minibuffer (not (null (active-minibuffer-window))))
  (setq persp-variables-has-minibuffer-focus (eq (active-minibuffer-window) (frame-selected-window)))
  (when (and persp-variables-has-minibuffer persp-variables-has-minibuffer-focus)
    (select-window (ivy--get-window ivy-last)))
  (when (and ivy-last persp-variables-has-minibuffer)
    (ivy-quit-and-run-keep-windows)) ; quit must run async.
  (ivy-cancel-timers)
  (swiper-async--remove-hooks)
  (put 'post-command-hook 'permanent-local nil)
  (remove-hook 'post-command-hook #'ivy--queue-exhibit)
  (pmv/store-current-state-in-persp))

(defun persp-variables-post-resume()
  (when (active-minibuffer-window)
    (with-ivy-window
      (when (and (not (null swiper--async-high-start-point))
                 (or (< swiper--async-high-start-point
                        swiper--async-high-end-point)
                     (< swiper--async-low-start-point
                        swiper--async-low-end-point)))
        (swiper--async-add-hooks)
        ; (add-hook 'post-command-hook #'ivy--queue-exhibit nil t)
        (schedule-isearch
         (current-buffer)
         'swiper--async-found-new-candidate)
        (swiper--async-update-input-ivy)))))

(defun persp-variables-after-activate-hook(frame-or-window)
  (run-at-time nil nil
    (lambda ()
      (pmv/restore-state-from-persp) ; should run after killing the minibuffer on deactivate.
      (when (and ivy-last persp-variables-has-minibuffer)
        (ivy-resume-keep-frame-selected-window
         (when persp-variables-has-minibuffer-focus
           (select-window (active-minibuffer-window)))
         (persp-variables-post-resume))))))

(add-hook 'persp-before-deactivate-functions #'persp-variables-before-deactivate-hook)
(add-hook 'persp-activated-functions #'persp-variables-after-activate-hook)


(push 'ivy-last pmv/cursor-specific-vars)
(push 'ivy-text pmv/cursor-specific-vars)
(push 'ivy--orig-cands pmv/cursor-specific-vars)
; (push 'ivy--all-candidates pmv/cursor-specific-vars) ; ???
; (push 'ivy--old-text pmv/cursor-specific-vars)
; (push 'ivy--old-re pmv/cursor-specific-vars)
; (push 'ivy-completing-read-ignore-handlers-depth pmv/cursor-specific-vars)
; (push 'ivy-highlight-grep-commands pmv/cursor-specific-vars)
; (push 'ivy--actions-list pmv/cursor-specific-vars)
; (push 'ivy--prompts-list pmv/cursor-specific-vars)
; (push 'ivy--sources-list pmv/cursor-specific-vars)
; (push 'ivy-current-prefix-arg pmv/cursor-specific-vars)
; (push 'current-prefix-arg pmv/cursor-specific-vars)
(push 'ivy-recursive-last pmv/cursor-specific-vars)
; (push 'ivy-recursive-restore pmv/cursor-specific-vars)
; (push 'inhibit-message pmv/cursor-specific-vars)
(push 'ivy-history pmv/cursor-specific-vars)
; (push 'ivy--directory pmv/cursor-specific-vars)
; (push 'ivy--length pmv/cursor-specific-vars)
(push 'ivy--index pmv/cursor-specific-vars)
; (push 'ivy--window-index pmv/cursor-specific-vars)
; (push 'ivy-exit pmv/cursor-specific-vars)
(push 'ivy--extra-candidates pmv/cursor-specific-vars) ;??
(push 'ivy-use-ignore pmv/cursor-specific-vars)
; (push 'ivy--default pmv/cursor-specific-vars)
; (push 'ivy--prompt-extra pmv/cursor-specific-vars)
; (push 'ivy--old-cands pmv/cursor-specific-vars)
; (push 'ivy--regex-function pmv/cursor-specific-vars)
; (push 'ivy--highlight-function pmv/cursor-specific-vars)
; (push 'ivy--subexps pmv/cursor-specific-vars)
; (push 'ivy--full-length pmv/cursor-specific-vars)
; (push 'ivy-case-fold-search pmv/cursor-specific-vars)
; (push 'ivy-read-action-format-function pmv/cursor-specific-vars)
(push 'ivy-completion-beg pmv/cursor-specific-vars)
(push 'ivy-completion-end pmv/cursor-specific-vars)
; (push 'ivy-marked-candidates pmv/cursor-specific-vars)
; (push 'ivy--regexp-quote pmv/cursor-specific-vars)
; (push 'Info-complete-menu-buffer pmv/cursor-specific-vars)
; (push 'ivy--regex-hash pmv/cursor-specific-vars)
; (push 'counsel-grep-last-line pmv/cursor-specific-vars)
; (push 'ivy--pulse-overlay pmv/cursor-specific-vars)

(push 'swiper--async-to-search pmv/cursor-specific-vars)
(push 'swiper--async-high-start-point pmv/cursor-specific-vars)
(push 'swiper--async-high-end-point pmv/cursor-specific-vars)
(push 'swiper--async-low-start-point pmv/cursor-specific-vars)
(push 'swiper--async-low-end-point pmv/cursor-specific-vars)
(push 'swiper--async-direction-backward pmv/cursor-specific-vars)



(push 'swiper--async-last-line pmv/cursor-clear-vars)
(push 'swiper--async-last-line-pos pmv/cursor-clear-vars)
(push 'ivy--last-cand pmv/cursor-clear-vars)
(push 'ivy--next-cand-index pmv/cursor-clear-vars)



(provide 'persp-mode-variables)
