(require 'comint)

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

(provide 'comint-ext)
