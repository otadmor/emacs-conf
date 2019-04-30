(require 'company)
(require 'auto-complete)

(require 'completion-epc)

(require 'python-mode)

(setq py-shell-completion-setup-code "")
(setq py-shell-module-completion-code "")
(setq py-ipython-module-completion-code "")
(setq py-ipython-module-completion-string "")

(defun py-shell-prefix()
  (and (or (eq major-mode 'py-python-shell-mode)
           (eq major-mode 'py-ipython-shell-mode)
           (eq major-mode 'shell-mode)
           )
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

(epc-completion-add 'shell-mode 'comint-mode-hook 'py-shell-prefix)
(epc-completion-add 'py-python-shell-mode 'py-python-shell-mode-hook 'py-shell-prefix)
(epc-completion-add 'py-ipython-shell-mode 'py-python-shell-mode-hook 'py-shell-prefix)

; (define-key py-python-shell-mode-map complete-key
;   (epc-completion-add 'py-python-shell-mode
;                       'py-python-shell-mode-hook
;                       'py-shell-prefix))

(defun py-shell-complete-substitute(&optional shell beg end word)
  (interactive)
  (auto-complete))
(defalias 'py-shell-complete 'py-shell-complete-substitute)

(setq py-ipython-command-args "--simple-prompt --nosep")
(provide 'company-py-shell)
