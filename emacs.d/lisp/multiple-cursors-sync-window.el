(require 'sync-window)


(defun mcsw/mode-enabled()
  (sync-window-mode 1))
(add-hook 'multiple-cursors-mode-enabled-hook 'mcsw/mode-enabled)


(defun mcsw/mode-disabled()
  (sync-window-mode 0))
(add-hook 'multiple-cursors-mode-disabled-hook 'mcsw/mode-disabled)


(provide 'multiple-cursors-sync-window)
