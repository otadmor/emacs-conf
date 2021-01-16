;;;  -*- lexical-binding: t -*-

;; (require 'company)

(defun company-ext--remove-props (str &rest props)
  "Return STR with text PROPS destructively removed."
  (remove-list-of-text-properties 0 (length str) props str)
  str)

(setq company--return-fullpath t)
(defun completion-file-name-table-fullpath-hook (orig-fun string pred action)
  (if (and company--return-fullpath
           (eq action t))
      (let ((res (funcall orig-fun string pred action)))
        (mapcar (lambda (a) (concat (file-name-directory string) a)) res))
    (funcall orig-fun string pred action)))

(defun company-completion-in-region (start end collection &optional predicate)
  (let* (
         (fixed-candidate-source
          (lambda (command &optional arg &rest ignored)
            (interactive (list 'interactive))
            (cl-case command
              ;; (interactive (company-begin-backend 'fixed-candidate-source))
              (prefix (buffer-substring-no-properties
                       (min start (buffer-end 1))
                       (min end (buffer-end 1))))
              (candidates (progn (let* (
                                        (cands
                                         (let ((company--return-fullpath t))
                                           (completion-all-completions
                                            arg
                                            collection predicate
                                            (length arg)))
                                         )
                                        (rev-arg (string-reverse arg))
                                        )
                                   (unless (null cands)
                                     (setcdr (last cands) nil))
                                   (dolist (s cands)
                                     (company-ext--remove-props s 'face))
                                   cands)))
              (meta (if (stringp arg) (get-text-property 0 'symbol arg)))
              (doc-buffer (company-doc-buffer
                           (if (stringp arg) (get-text-property 0 'document arg))))
              (annotation (if (stringp arg) (get-text-property 0 'summary arg)))
              (location nil)
              (no-cache t)
              (sorted t))
            ))
         )
    (company-begin-backend fixed-candidate-source)))

(with-eval-after-load 'company
  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-continue-commands 'comint-previous-matching-input-from-input t)
  (add-to-list 'company-continue-commands 'comint-next-matching-input-from-input t)
  (advice-add 'completion-file-name-table :around #'completion-file-name-table-fullpath-hook)
  (setq company-minimum-prefix-length 100)
  (setq company-require-match nil)

  (setq company-orig--completion-in-region-function
        'completion--in-region)
  (defun completion-in-region-company-or-ivy (start end collection &optional predicate)
    (if (or (not company-mode)
            (eq (selected-window) (active-minibuffer-window)))
        (funcall company-orig--completion-in-region-function
                 start end collection predicate)
      (company-completion-in-region start end collection predicate)))

  (define-key company-active-map (kbd "C-i") (lambda()
                                               (interactive)
                                               (let ((completion-in-region-function company-orig--completion-in-region-function))
                                                 (completion-at-point))))

  (require 'company-quickhelp)
  (with-eval-after-load 'company-quickhelp
    (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
    (setq company-quickhelp-delay 0.1)
    (company-quickhelp-mode))

  (add-hook 'after-init-hook
            (lambda ()
              (setq company-orig--completion-in-region-function
                    completion-in-region-function)
              (setq completion-in-region-function
                    'completion-in-region-company-or-ivy))))

(provide 'company-ext)
