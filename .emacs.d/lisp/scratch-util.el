;; display stuff
(defun init-scratch()
  (save-excursion
    (switch-to-buffer "*scratch*")
    (setf (buffer-string) "")
    (insert ";;")
    (newline)
    (insert ";; Welcome h4x0r !!!")
    (newline)
    (insert ";;")
    (newline)))

(defun dont-kill-scratch ()
  (if (not (equal (buffer-name) "*scratch*"))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (init-scratch)
    (bury-buffer)
    nil))

(add-hook 'after-init-hook
          (lambda ()
            (init-scratch)
            (add-hook 'kill-buffer-query-functions #'dont-kill-scratch)))

(provide 'scratch-util)
