(require 'cl-lib)
(require 'company)

(require 'completion-epc)

(require 'python-mode)

(setq py-shell-completion-setup-code (concat "import sys; sys.path.append('" (file-name-directory load-file-name) "'); import py_epc_completion; _=sys.path.pop();"))

(defun company-py-shell-prefix()
  (and (eq major-mode 'py-python-shell-mode)
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
  (epc-complete arg))

(defun company-py-shell (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-py-shell))
    (prefix (company-py-shell-prefix))
    (candidates (company-py-shell-candidates arg))))

(defun py-shell-complete-substitute(&optional shell beg end word) (interactive)
       (company-py-shell-candidates (company-py-shell-prefix)))
(defalias 'py-shell-complete 'py-shell-complete-substitute)

(setq py-ipython-command-args "--simple-prompt --nosep")
(provide 'company-py-shell)
