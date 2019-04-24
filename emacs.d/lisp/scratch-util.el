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
(init-scratch)

(add-hook 'kill-buffer-query-functions #'dont-kill-scratch)
(defun dont-kill-scratch ()
  (if (not (equal (buffer-name) "*scratch*"))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (init-scratch)
    (bury-buffer)
    nil))

(provide 'scratch-util)
