
(defun send-shell-command(command)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (user-error "Current buffer has no process")
      (funcall comint-input-sender proc command))))

(defun new-shell ()
  "create new shell"
  (interactive)
  (let (
        (b (generate-new-buffer "*shell*"))
        )
    (switch-to-buffer b)
    (shell b) b))

(defun new-shell-with-dir(dir)
  (message "Opening shell on %S" dir)
  (let (
        (b (generate-new-buffer "*shell*"))
        )
    (switch-to-buffer b)
    (setq default-directory dir)
    (shell b) b))

(defun shell-command-on-region-inplace ()
  (interactive)
  (let ((command (read-shell-command "Shell command on region: ")))
    (shell-command-on-region (region-beginning) (region-end) command 1)))

(defun shell-change-to-current-dir ()
  (interactive)
  (let ((current-directory-path (expand-file-name default-directory)))
    (shell)
    (let (
          (cd-command (concat "cd "
                              (shell-quote-argument current-directory-path)))
          )
      (send-shell-command cd-command)))
  (end-of-buffer))

(defun old-shell-with-dir(current-directory-path)
  (select-old-or-create-new
   'shell-mode
   (lambda () (new-shell-with-dir current-directory-path))
   (lambda ()
     (let (
           (cd-command (concat "cd "
                               (shell-quote-argument current-directory-path)))
           )
       (send-shell-command cd-command))
     (end-of-buffer))))

;; (defun old-shell-with-dir(current-directory-path)
;;   (let (
;;         (shb (car (get-buffers-with-major-mode 'shell-mode)))
;;         )
;;     (if (eq shb nil)
;;         (new-shell-with-dir current-directory-path)
;;       (switch-to-buffer shb)
;;       (let (
;;             (cd-command (concat "cd "
;;                               (shell-quote-argument current-directory-path)))
;;           )
;;         (send-shell-command cd-command))
;;       (end-of-buffer))
;;     shb))

(defun old-shell() (interactive)
  (old-shell-with-dir (expand-file-name default-directory)))

(defun track-shell-directory/procfs ()
  (shell-dirtrack-mode 0)
  (add-hook 'comint-preoutput-filter-functions
            (lambda (str)
              (prog1 str
                (when (string-match comint-prompt-regexp str)
                  (cd (file-symlink-p
                       (format "/proc/%s/cwd" (process-id
                                               (get-buffer-process
                                                (current-buffer)))))))))
            nil t))

(add-hook 'shell-mode-hook 'track-shell-directory/procfs)

(provide 'shell-ext)
