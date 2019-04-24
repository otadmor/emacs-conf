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
    (comment-or-uncomment-region beg end 1)))

(defun my-toggle-truncate-lines ()
  "Toggle truncate lines in quietly."
  (interactive)
  (let ((inhibit-message t))
    (toggle-truncate-lines)))

(provide 'utils)
