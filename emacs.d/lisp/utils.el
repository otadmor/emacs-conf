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

(provide 'utils)
