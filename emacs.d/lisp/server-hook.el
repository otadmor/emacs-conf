(defun check-frames-connection ()
  (when (functionp 'x-check-frame)
    (dolist (frame (frame-list))
      (when (eq (framep frame) 'x)
        (x-check-frame frame)))))
(defun check-frames-connection-timer ()
  (check-frames-connection)
  (run-at-time 2 nil 'check-frames-connection-timer))
(run-at-time 2 nil 'check-frames-connection-timer)

(setq server-inside-emacs-client nil)
; patches the call at server.el:1237
(defun server-create-window-system-frame-hook(orig-fun &rest args)
  (setq server-inside-emacs-client t)
  (check-frames-connection)
  (apply orig-fun args)
;;   (let (
;;         (expected-display (car-safe args))
;;         (frame-display (frame-parameter (selected-frame) 'display))
;;         (other-arguments (cdr-safe args))
;;         )
;;
;; ;  (message "Create server params: %S, frame params: %S" args frame-display)
;; ;  (message "CAR %S is iq %S" expected-display (string-equal (car-safe args) "localhost:current"))
;; ;  (message "CDR %S" other-arguments)
;; ;  (message "REJOIN %S" (cons frame-display other-arguments))
;;   (if (null frame-display)
;;       (if (string-equal expected-display "localhost:current")
;;           (apply orig-fun (cons (getenv "DISPLAY") other-arguments))
;;         (apply orig-fun args)
;;         )

;;     (if (string-equal expected-display "localhost:current")
;;         (apply orig-fun (cons frame-display other-arguments))
;;       (apply orig-fun args)
;;       )
;;     )
;;   )

  )

(advice-add 'server-create-window-system-frame :around #'server-create-window-system-frame-hook)

(defun exit-emacs-or-close-frame() (interactive)
       (if server-inside-emacs-client (delete-frame) (save-buffers-kill-emacs)))

(provide 'server-hook)
