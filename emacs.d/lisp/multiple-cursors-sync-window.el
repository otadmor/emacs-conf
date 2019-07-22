(require 'sync-window)


(defun mcsw/mode-enabled()
  (sync-window-mode 1))

(defun mcsw/mode-disabled()
  (sync-window-mode 0))

(with-eval-after-load 'sync-window
  (with-eval-after-load 'multiple-cursors
    (add-hook 'multiple-cursors-mode-enabled-hook 'mcsw/mode-enabled)
    (add-hook 'multiple-cursors-mode-disabled-hook 'mcsw/mode-disabled))

(provide 'multiple-cursors-sync-window)
