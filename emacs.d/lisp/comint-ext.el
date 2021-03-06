;; (require 'comint)

(defun comint-delete-backward-char(fun &rest args)
  (when (or (region-active-p)
            (not (consp comint-last-prompt))
            (/= (cdr comint-last-prompt) (point)))
    (apply fun args)))

(with-eval-after-load 'comint

  (setq comint-get-old-input (lambda () ""))
  (ansi-color-for-comint-mode-on)

  (setq comint-password-prompt-regexp
        (concat comint-password-prompt-regexp
                "\\|^Password for \\\\'.*\\\\':\\s *\\'"))
  (setq comint-password-prompt-regexp
        (concat comint-password-prompt-regexp
                "\\|^Password for '.*':\s+"))
  (setq comint-password-prompt-regexp
        (concat comint-password-prompt-regexp
                "\\|^.*\\\\' password:\\s *\\'"))

  (advice-add 'delete-backward-char :around #'comint-delete-backward-char))

(provide 'comint-ext)
