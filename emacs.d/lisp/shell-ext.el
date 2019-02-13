
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


(ansi-color-for-comint-mode-on)

(provide 'shell-ext)
