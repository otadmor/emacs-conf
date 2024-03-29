;; -*- lexical-binding: t; -*-
;; (require 'which-func)

(defun update-next-error-which-function ()
  (which-func-update-1 (selected-window)))

(defun which-func-ff-hook ()
  "File find hook for Which Function mode.
It creates the Imenu index for the buffer, if necessary."
  (if (not (or (null which-func-maxout)
               (< buffer-saved-size which-func-maxout)
               (= which-func-maxout 0)))
      (setq which-func-mode nil)
    (which-func-try-to-enable)
    (condition-case err
        (if (and which-func-mode
                 (not (member major-mode which-func-non-auto-modes))
                 )
            (setq imenu--index-alist
                  (save-excursion (funcall imenu-create-index-function))))
      (imenu-unavailable
       (setq which-func-mode nil))
      (error
       (message "which-func-ff-hook error: %S" err)
       (setq which-func-mode nil)))))

(defun last-buffer() (interactive) (switch-to-buffer (other-buffer)))
(defun next-buff() (interactive) (other-window 1))
(defun prev-buffer() (interactive) (other-window -1))
(defun dont-kill-emacs() (interactive) (message "Use C-x c to leave"))
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (when (active-minibuffer-window)
      (if (eq (active-minibuffer-window) (frame-selected-window))
          (select-window (get-mru-window))
          (select-window (active-minibuffer-window))
          )))

(defun lines-in-region () (interactive)
       (message "lines %S" (count-lines (region-beginning) (region-end))))

(defun my-comment-or-uncomment-region ()
  (interactive)
  (let ((beg (save-excursion (when mark-active (goto-char (region-beginning)))
                             (line-beginning-position)))
	(end (save-excursion (when mark-active (goto-char (region-end)))
                             (line-end-position))))
    (when (and mark-active
               (< beg end)
               (= (save-excursion (goto-char end)
                                  (line-beginning-position)) (region-end)))
      (setq end (- (region-end) 1)))
    (comment-or-uncomment-region beg end)))

(defun my-toggle-truncate-lines ()
  "Toggle truncate lines in quietly."
  (interactive)
  (let ((inhibit-message t))
    (toggle-truncate-lines)))

(defun new-buffer-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (switch-to-buffer buffer)))

(put 'erase-buffer 'disabled nil)


(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
     major-mode))

(defun get-buffers-with-major-mode (find-major-mode)
  "Get a list of buffers in which minor-mode is active"
  (interactive)
  (let (
        (persp (when (and (boundp 'persp-mode) persp-mode) (get-current-persp)))
        (major-mode-buffers)
        )
    (dolist (buf (buffer-list) major-mode-buffers)
      (when (and (with-current-buffer buf (eq major-mode find-major-mode))
                 (or (not (and (boundp 'persp-mode) persp-mode))
                     (persp-contain-buffer-p buf persp)))
        (push buf major-mode-buffers)))))

(defun select-old-or-create-new(major-mode create-func existing-func)
  (interactive)
  (let (
        (buffer (car (get-buffers-with-major-mode major-mode)))
        )
    (if (eq buffer nil)
        (funcall create-func)
      (let (
            (window (get-buffer-window buffer))
            )
        (if (not (null window))
            (select-window window)
          (switch-to-buffer buffer))
        (funcall existing-func)
        buffer))))


;; disable error messages
(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (cond
   ((eq (car data) 'beginning-of-buffer) (goto-char (point-min)))
   ((eq (car data) 'end-of-buffer) (goto-char (point-max)))
   (t (command-error-default-function data context caller))))
(setq command-error-function #'my-command-error-function)

(defun ignore-errors-hook (orig-fun &rest args)
  (ignore-errors
    (apply orig-fun args)))

(setq callid 0)
(defun log-func (fn)
  (advice-add fn :around (lambda (orig-fun &rest args)
                           (cl-incf callid)
                           (message "%S: %S %S" callid fn args)
                           (let (
                                 (res (apply orig-fun args))
                                 )
                             (message "%S: %S returned %S" callid fn res)
                             res))))

(with-eval-after-load 'which-func
  (add-hook 'next-error-hook 'update-next-error-which-function))

(defun wrap-file-name (file-name)
  (if (file-remote-p default-directory)
      (with-parsed-tramp-file-name default-directory nil
        (tramp-make-tramp-file-name
         (tramp-file-name-method v)
         (tramp-file-name-user v)
         (tramp-file-name-domain v)
         (tramp-file-name-host v)
         (tramp-file-name-port v)
         file-name))
    file-name))

(defun read-file-lines-reversed (file-name)
  (nreverse (split-string
             (with-temp-buffer
               (insert-file-contents file-name)
               (buffer-substring-no-properties
                (point-min)
                (point-max)))
             "\n" t)))

(provide 'utils)
