
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

(defun text-has-property (START END PROP &optional OBJECT)
  (or (not (null (get-char-property START PROP OBJECT)))
      (< (next-property-change START OBJECT END) END)))

(defun font-lock-prepend-text-property--hook (orig-fun START END PROP VALUE &optional OBJECT)
  (when (and (eq major-mode 'shell-mode)
             (eq VALUE 'comint-highlight-prompt))
    ;; (message "CURRENT DIR %S START %S END %S CURRENT LINE START %S BUFFER END %S" default-directory START END (point) (point-max))
    (let ((old-default-directory-overlay (default-directory-overlay-at-point START)))
      (when old-default-directory-overlay
        (move-overlay old-default-directory-overlay
                      (overlay-start old-default-directory-overlay)
                      (1- START))))
    (let ((default-directory-overlay (make-overlay START (point) nil t t)))
      ;; default-directory is still the directory of the previous command
      (overlay-put default-directory-overlay 'type 'default-directory)
      (overlay-put default-directory-overlay 'evaporate t)
      (overlay-put default-directory-overlay 'default-directory default-directory)))
  (when (or (not (eq major-mode 'shell-mode))
            (not (and (eq PROP 'font-lock-face)
                      (eq VALUE 'comint-highlight-prompt)))
            (null (text-has-property START END 'ansi-color-face)))
    (funcall orig-fun START END PROP VALUE OBJECT)))

(defun default-directory-overlay-at-point (point)
  (let ((overlays (overlays-at point))
        found)
    (while (and overlays
                (not found))
      (let ((overlay (car overlays)))
        (if (eq (overlay-get overlay 'type) 'default-directory)
            (setq found overlay)))
      (setq overlays (cdr overlays)))
    found))

(defun default-directory-at-point (point)
  (interactive "d")
  (let ((default-directory-overlay (default-directory-overlay-at-point point)))
    (if default-directory-overlay
        (overlay-get default-directory-overlay 'default-directory)
      default-directory)))

(with-eval-after-load 'shell
  ;; (add-hook 'shell-mode-hook 'track-shell-directory/procfs)
  (advice-add 'font-lock-prepend-text-property :around #'font-lock-prepend-text-property--hook))

(provide 'shell-ext)
