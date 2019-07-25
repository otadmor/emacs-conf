;;;  -*- lexical-binding: t -*-

;; (require 'company)

(defun company-ext--remove-props (str &rest props)
  "Return STR with text PROPS destructively removed."
  (remove-list-of-text-properties 0 (length str) props str)
  str)

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
              (candidates (progn (let (
                                       (cands
                                        (completion-all-completions
                                         arg
                                         collection predicate
                                         (length arg))
                                         ;; (- (min end (buffer-end 1)) (min start (buffer-end 1))))
                                        )
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
  (setq company-minimum-prefix-length 0)
  (setq company-require-match nil)

  (setq company-orig--completion-in-region-function
        'completion--in-region)
  (defun completion-in-region-company-or-ivy (start end collection &optional predicate)
    (if (eq (selected-window) (active-minibuffer-window))
        (funcall company-orig--completion-in-region-function
                 start end collection predicate)
      (message "not in mini")
      (company-completion-in-region start end collection predicate)))

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
