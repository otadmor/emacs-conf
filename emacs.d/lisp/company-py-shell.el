;;;  -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'company)

(require 'completion-epc)

(require 'python-mode)

(setq py-shell-completion-setup-code (concat "import sys; sys.path.append('" (file-name-directory load-file-name) "'); import py_epc_completion; _=sys.path.pop();"))
(setq py-shell-module-completion-code "")
(setq py-ipython-module-completion-code "")
(setq py-ipython-module-completion-string "")

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

(defun company-py-shell-collect-candidates (completion)
  "Return a candidate from a COMPLETION reply."
  (let ((candidate (plist-get completion :word)))
    (when candidate
      (put-text-property 0 1 :doc (plist-get completion :doc) candidate)
      (put-text-property 0 1 :symbol (plist-get completion :symbol) candidate)
      (put-text-property 0 1 :description (plist-get completion :description) candidate)
      candidate)))

(defun company-py-shell-meta (candidate)
  "Return company meta string for a CANDIDATE."
  (get-text-property 0 :description candidate))

(defun company-py-shell-symbol (candidate)
  "Return company annotation string for a CANDIDATE."
  (format "[%s]" (get-text-property 0 :symbol candidate)))

(defun company-py-shell-doc (candidate)
  "Return a company documentation buffer from a CANDIDATE."
  (company-doc-buffer (get-text-property 0 :doc candidate)))


(defun company-py-shell-candidates(prefix)
  (epc-complete prefix))

(defun company-py-shell-complete-deferred(prefix)
  (cons :async
        (lambda (callback)
          (deferred:nextc
            (epc-complete-deferred prefix)
            (lambda (reply)
              (let ((candidates (mapcar 'company-py-shell-collect-candidates reply)))
                (funcall callback candidates)))))))

(defun company-py-shell (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-py-shell))
    (prefix (company-py-shell-prefix))
    (candidates (company-py-shell-complete-deferred arg))
    (meta (company-py-shell-meta arg))
    (doc-buffer (company-py-shell-doc arg))
    (annotation (company-py-shell-symbol arg))
    (location nil)
    (sorted t)))

(defun py-shell-complete-substitute(&optional shell beg end word) (interactive)
       (company-py-shell-candidates (company-py-shell-prefix)))
(defalias 'py-shell-complete 'py-shell-complete-substitute)

(setq py-ipython-command-args "--simple-prompt --nosep")
(provide 'company-py-shell)
