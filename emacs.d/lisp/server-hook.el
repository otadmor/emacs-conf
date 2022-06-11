;; -*- lexical-binding: t; -*-

;; (require 'server)

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

(defun server-sentinel-hook (&rest _)
  (safe-check-frames-connection))

(setq server-inside-emacs-client nil)
(defun server-create-window-system-frame-hook(orig-fun &rest args)
  (setq server-inside-emacs-client t)
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
          (frame (apply orig-fun (cons display other-arguments)))
          )
      (run-at-time 0 nil (lambda ()
                           (with-selected-frame frame
                             (lockstep-and-prepare-persp))))
      frame)))

;; (defun server-create-tty-frame-hook(orig-fun &rest args)
;;   (setq server-inside-emacs-client t)
;;   (let (
;;         (frame (apply orig-fun args))
;;         )
;;     (run-at-time 0 nil (lambda ()
;;                          (with-selected-frame frame
;;                            (lockstep-and-prepare-persp))))
;;     frame))
(defun server-create-tty-frame-hook(orig-fun &rest args)
  (let ((frame (apply 'server-create-window-system-frame-hook orig-fun args)))
    ;(define-key input-decode-map "\eD" [left])
    ;(define-key input-decode-map "\eC" [right])
    ;; (define-key input-decode-map "\e[1;2D" [C-left])
    ;; (define-key input-decode-map "\e[1;2C" [C-right])
    ;; (define-key input-decode-map "\e[1;2A" [C-up])
    ;; (define-key input-decode-map "\e[1;2B" [C-down])
    (define-key input-decode-map "\e[D" [C-left])
    (define-key input-decode-map "\e[C" [C-right])
    (define-key input-decode-map "\e[A" [C-up])
    (define-key input-decode-map "\e[B" [C-down])
    (define-key input-decode-map (kbd "ESC M-O D") [(meta left)])
    (define-key input-decode-map (kbd "ESC M-O C") [(meta right)])
    (define-key input-decode-map (kbd "ESC M-O A") [(meta up)])
    (define-key input-decode-map (kbd "ESC M-O B") [(meta down)])

    ;(define-key input-decode-map "\eOD" [S-left])
    ;(define-key input-decode-map "\eOC" [S-right])
    frame))

(defun save-persp-on-delete-frame (frame)
  (condition-case nil
      (if persp-mode
          (persp-save-state-to-file)
        (when (functionp 'lockstep-store)
          (lockstep-store)
          (turn-off-lockstep)))
    (error nil)))



;; (require 'lockstep)
(defun lockstep-and-prepare-persp () ;;persp-file phash persp-names)
  (with-eval-after-load 'persp-mode
    (let* (
           (this-frame (selected-frame))
           (x-frame-list (remove-if
                          (lambda (frame) (or (not (eq (framep frame) 'x))
                                              (equal this-frame frame)))
                          (frame-list)))
           )
      (when persp-mode
        (if (null x-frame-list)
            (perspsw1)
          (let* (
                 (first-frame (car x-frame-list))
                 (persp (get-frame-persp first-frame))
                 (persp-name (safe-persp-name persp))
                 )
            (persp-switch persp-name))))))
  (with-eval-after-load 'lockstep
    (lockstep)))

(defun exit-emacs-or-close-frame() (interactive)
       (if server-inside-emacs-client (delete-frame) (save-buffers-kill-emacs)))

(with-eval-after-load 'server
  (advice-add 'server-sentinel :before #'server-sentinel-hook)
  (advice-add 'server-create-window-system-frame :around #'server-create-window-system-frame-hook)
  (advice-add 'server-create-tty-frame :around #'server-create-tty-frame-hook)
  (setq initial-buffer-choice (lambda () (current-buffer)))

  (with-eval-after-load 'persp-mode
    (add-hook 'delete-frame-functions #'save-persp-on-delete-frame)))

(provide 'server-hook)
