(require 'cl-lib)
(require 'python-mode)
(require 'company)

(defun company-py-shell-prefix()
  (and (eq major-mode 'py-python-shell-mode)
;       (not (string-prefix-p "..." (thing-at-point 'line 1)))
       (let* ((exception-buffer (current-buffer))
              (pos (copy-marker (point)))
              (pps (parse-partial-sexp (or (ignore-errors (overlay-end comint-last-prompt-overlay))(line-beginning-position)) (point)))
              (in-string (when (nth 3 pps) (nth 8 pps)))
              (beg
               (save-excursion
                 (and in-string
                      ;; possible completion of filenames
                      (progn
                        (goto-char in-string)
                        (and
                         (save-excursion
                           (skip-chars-backward "^ \t\r\n\f")(looking-at "open")))
                        (skip-chars-forward "\"'")(point)))
                 (progn (and (eq (char-before) ?\()(forward-char -1))
                        (skip-chars-backward "a-zA-Z0-9_.'") (point))))
              (end (point))
              (word (buffer-substring-no-properties beg end)))
         word)))


(defun company-py-shell-candidates(arg)
  (message "completing %S" arg)
  (setq py-last-window-configuration
        (current-window-configuration))
  (let (
        (ausdruck (and (string-match "^/" arg)(setq arg (substring-no-properties arg 1))(concat "\"" arg "*\"")))
        )
    (let (
        ;; when in string, assume looking for filename
        (filenames (and ausdruck
                        (list (replace-regexp-in-string "\n" "" (shell-command-to-string (concat "find / -maxdepth 1 -name " ausdruck))))))
        (imports (py-find-imports))
        (exception-buffer (current-buffer))
        (shell (py-choose-shell))
        (input arg)
        )
    (let (
          (proc (or
                 ;; completing inside a shell
                 (get-buffer-process exception-buffer)
                 (and (comint-check-proc shell)
                      (get-process shell))
                 (prog1
                     (get-buffer-process (py-shell nil nil shell))
                   (sit-for py-new-shell-delay))))
          (code (if (string-match "[Ii][Pp]ython*" shell)
                    (py-set-ipython-completion-command-string shell)
                  py-shell-module-completion-code))
          )
      (when imports
        (py--send-string-no-output imports proc))
      ;; (py--delay-process-dependent proc)
      (sit-for 0.1 t)
      (let* ((completion
              (py--shell-completion-get-completions
               input proc code)))
        (nconc completion filenames))))))


(defun company-py-shell (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-py-shell))
    (prefix (company-py-shell-prefix))
    (candidates (company-py-shell-candidates arg))))

(defun py-shell-complete(&optional shell beg end word) (interactive)
       (company-py-shell-candidates (company-py-shell-prefix)))

(setq py-ipython-command-args "--simple-prompt --nosep")
(provide 'company-py-shell)
