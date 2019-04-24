
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
    (end-of-buffer)
    (insert-string "cd " )
    (insert-string (shell-quote-argument current-directory-path))
    (comint-send-input)))


(defun old-shell-with-dir(current-directory-path)
  (let (
        (shb (car (get-buffers-with-major-mode 'shell-mode)))
        )
    (if (eq shb nil)
        (new-shell-with-dir current-directory-path)
      (switch-to-buffer shb)
      (end-of-buffer)
      (insert-string "cd " )
      (insert-string (shell-quote-argument current-directory-path))
      (comint-send-input))
    shb))

(defun old-shell() (interactive)
  (old-shell-with-dir (expand-file-name default-directory)))

(provide 'shell-ext)
