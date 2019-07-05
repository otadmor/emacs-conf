;; -*- lexical-binding: t; -*-
(require 'persp-mode)

;; COPIED FROM multiple-cursors-core.el
(setq persp-variables-has-minibuffer nil)
(setq persp-variables-has-minibuffer-focus nil)

(defvar pmv/specific-vars nil
  "A list of vars that need to be tracked on a per-perspective basis.")

(defvar pmv/saved-specific-vars nil
  "A list of vars that need to be tracked on a per-perspective basis.")

(defvar pmv/clear-vars nil
  "A list of vars that will be set to nil when switching back to a perspective.")

(defcustom persp-variables-restored nil
  "Hooks to runs after loading variable for a perspective."
  :group 'persp-mode
  :type 'hook)

(push 'persp-variables-has-minibuffer pmv/specific-vars)
(push 'persp-variables-has-minibuffer-focus pmv/specific-vars)



; (defun* pmv/store-current-state-in-persp (&optional (persp (get-current-persp)))
;   (dolist (var pmv/specific-vars)
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
;   (dolist (var pmv/specific-vars)
;     (if (persp-has-parameter var persp)
;         (set var (persp-parameter var persp))
;       (makunbound var)))
;   persp)

; (defun ivy-state-collection(x) )

(defun persp-elisp-object-readable-p-hook (orig-fun &rest args)
  (let (
        (param (car args))
        )
    (if (consp param)
        (let (
              (param-name (car param))
              )
          (when (not (or (memq param-name pmv/specific-vars)
                         (memq param-name pmv/clear-vars)))
            (apply orig-fun args)))
      (apply orig-fun args))))
(advice-add 'persp-elisp-object-readable-p :around #'persp-elisp-object-readable-p-hook)


(defun* pmv/store-current-state-in-persp (&optional (persp (get-current-persp)))
  (dolist (var pmv/specific-vars)
    (when (boundp var) (set-persp-parameter var (symbol-value var) persp)))
  (dolist (var pmv/saved-specific-vars)
    (when (boundp var) (set-persp-parameter var (symbol-value var) persp)))
  persp)

(defun* pmv/restore-state-from-persp (&optional (persp (get-current-persp)))
  (dolist (var pmv/specific-vars)
    (set var (persp-parameter var persp)))
  (dolist (var pmv/saved-specific-vars)
    (set var (persp-parameter var persp)))
  (dolist (var pmv/clear-vars)
    (set var nil))
 (run-hook-with-args 'persp-variables-restored)
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
                        (setq ivy-index--persp-variables ivy--index)
                        (let (
                              (this-command 'ivy-resume)
                              )
                          (ivy-resume))))))))

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
      (schedule-isearch
       (current-buffer)
       'swiper--async-found-new-candidate)
      (swiper--async-update-input-ivy))))

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


(push 'ivy-last pmv/specific-vars)
(push 'ivy-text pmv/specific-vars)
(push 'ivy--orig-cands pmv/specific-vars)
; (push 'ivy--all-candidates pmv/specific-vars) ; ???
; (push 'ivy--old-text pmv/specific-vars)
; (push 'ivy--old-re pmv/specific-vars)
; (push 'ivy-completing-read-ignore-handlers-depth pmv/specific-vars)
; (push 'ivy-highlight-grep-commands pmv/specific-vars)
; (push 'ivy--actions-list pmv/specific-vars)
; (push 'ivy--prompts-list pmv/specific-vars)
; (push 'ivy--sources-list pmv/specific-vars)
; (push 'ivy-current-prefix-arg pmv/specific-vars)
; (push 'current-prefix-arg pmv/specific-vars)
(push 'ivy-recursive-last pmv/specific-vars)
; (push 'ivy-recursive-restore pmv/specific-vars)
; (push 'inhibit-message pmv/specific-vars)
(push 'ivy-history pmv/specific-vars)
; (push 'ivy--directory pmv/specific-vars)
; (push 'ivy--length pmv/specific-vars)
(push 'ivy--index pmv/specific-vars)
; (push 'ivy--window-index pmv/specific-vars)
; (push 'ivy-exit pmv/specific-vars)
(push 'ivy--extra-candidates pmv/specific-vars) ;??
(push 'ivy-use-ignore pmv/specific-vars)
; (push 'ivy--default pmv/specific-vars)
; (push 'ivy--prompt-extra pmv/specific-vars)
; (push 'ivy--old-cands pmv/specific-vars)
; (push 'ivy--regex-function pmv/specific-vars)
; (push 'ivy--highlight-function pmv/specific-vars)
; (push 'ivy--subexps pmv/specific-vars)
; (push 'ivy--full-length pmv/specific-vars)
; (push 'ivy-case-fold-search pmv/specific-vars)
; (push 'ivy-read-action-format-function pmv/specific-vars)
(push 'ivy-completion-beg pmv/specific-vars)
(push 'ivy-completion-end pmv/specific-vars)
; (push 'ivy-marked-candidates pmv/specific-vars)
; (push 'ivy--regexp-quote pmv/specific-vars)
; (push 'Info-complete-menu-buffer pmv/specific-vars)
; (push 'ivy--regex-hash pmv/specific-vars)
; (push 'counsel-grep-last-line pmv/specific-vars)
; (push 'ivy--pulse-overlay pmv/specific-vars)

(push 'swiper--async-to-search pmv/specific-vars)

(push 'swiper--async-direction-backward pmv/specific-vars)



(push 'swiper--async-last-line pmv/clear-vars)
(push 'swiper--async-last-line-pos pmv/clear-vars)
(push 'ivy--last-cand pmv/clear-vars)
(push 'ivy--next-cand-index pmv/clear-vars)

(push 'recentf-list pmv/specific-vars)

;; (push 'sr-speedbar-width pmv/saved-specific-vars)

(provide 'persp-mode-variables)
