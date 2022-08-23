;; (require 'comint)
;; (require 'url-tramp)
(defun comint-osc-directory-tracker (_ text)
  "Update `default-directory' from OSC 7 escape sequences.

This function is intended to be included as an entry of
`comint-osc-handlers'.  You should moreover arrange for your
shell to print the appropriate escape sequence at each prompt,
say with the following command:

    printf \"\\e]7;file://%s%s\\e\\\\\" \"$HOSTNAME\" \"$PWD\"

This functionality serves as an alternative to `dirtrack-mode'
and `shell-dirtrack-mode'."
  ;; TODO use : url-tramp-convert-url-to-tramp
  (let ((fullpath (let ((url (url-generic-parse-url text)))
                    (let ((is-filescheme (string= (url-type url) "file"))
                          (is-localhost (or (null (url-host url))
                                            (cl-equalp (url-host url) (system-name))))
                          (is-sameuser (or (null (url-user url))
                                           (string= (url-user url) user-login-name))))
                      (if (and is-filescheme is-localhost is-sameuser)
                          (url-filename url)
                        (tramp-make-tramp-file-name (make-tramp-file-name
                                                     :method (if is-filescheme
                                                                 (if is-localhost "sudo" "ssh")
                                                               (url-type url))
                                                     :user (if is-filescheme
                                                               (or (url-user url) user-login-name)
                                                             (url-user url))
                                                     :host (if is-filescheme
                                                               (or (url-host url) "localhost")
                                                             (url-host url))
                                                     :port (url-port-if-non-default url)
                                                     ;; url-unhex-string
                                                     :localname (url-filename url))))))))
    (when fullpath
      ;; (message "FULL %S" fullpath)
      (ignore-errors (cd-absolute fullpath)))))

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

  (advice-add 'delete-backward-char :around #'comint-delete-backward-char)

;; (add-hook 'comint-mode-hook (lambda ()
;;                               (setenv "PROMPT_COMMAND" (format "history -a"))))
  (shell-dirtrack-mode 0)
  (dirtrack-mode 0)
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output))
(provide 'comint-ext)
