(defun check-frames-connection ()
  (when (functionp 'x-check-frame)
    (dolist (frame (frame-list))
      (when (eq (framep frame) 'x)
        (x-check-frame frame)))))
(defun check-frames-connection-timer ()
  (safe-check-frames-connection)
  (run-at-time 2 nil 'check-frames-connection-timer))
(run-at-time 2 nil 'check-frames-connection-timer)

(defun safe-x-check-frame (frame)
  (condition-case nil
      (x-check-frame frame)
    (error nil)))

(defun safe-check-frames-connection ()
  (when (functionp 'x-check-frame)
    (dolist (frame (frame-list))
      (when (eq (framep frame) 'x)
        (safe-x-check-frame frame)))))


(setq server-inside-emacs-client nil)
(defun server-create-window-system-frame-hook(orig-fun &rest args)
  (setq server-inside-emacs-client t)
  (check-frames-connection)
  (let* (
         (expected-display (car-safe args))
         (frame-display (unless (null (selected-frame))
                          (progn ;when (safe-x-check-frame (selected-frame))
                            (frame-parameter (selected-frame) 'display))))
         (other-arguments (cdr-safe args))
         (proc (cadr other-arguments))
         (proc-env (process-get proc 'env))
         (env-display (getenv-internal "DISPLAY" (process-get proc 'env)))
         (display (if (not (string-equal expected-display "localhost:current"))
                      expected-display
                    (if (null env-display)
                        (if (null (getenv "DISPLAY"))
                            frame-display
                          (getenv "DISPLAY"))
                      env-display)))
        )
    ;;  (message "Create server params: %S, frame params: %S" args frame-display)
    ;;  (message "CAR %S is iq %S" expected-display (string-equal (car-safe args) "localhost:current"))
    ;;  (message "CDR %S" other-arguments)
    ;;  (message "REJOIN %S" (cons frame-display other-arguments))
    (let (
          (res (apply orig-fun (cons display other-arguments)))
          )
      (condition-case nil
          (persp-load-state-from-file)
        (error nil))
      (setq tty-name nil)
      res)))
(advice-add 'server-create-window-system-frame :around #'server-create-window-system-frame-hook)


(defun exit-emacs-or-close-frame() (interactive)
       (if server-inside-emacs-client (delete-frame) (save-buffers-kill-emacs)))

(provide 'server-hook)
