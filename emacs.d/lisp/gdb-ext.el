(require 'gdb)
(defun gud-gdb-marker-filter-hook(orig-fun &rest args)
  (let (
        (res (apply orig-fun args))
        )
    (ansi-color-process-output res)
    )
)
(advice-add 'gud-gdb-marker-filter :around #'gud-gdb-marker-filter-hook)


(provide 'gdb-ext)
