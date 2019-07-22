;;;  -*- lexical-binding: t -*-

(require 'company)

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
                                     (ivy--remove-props s 'face))
                                   cands)))
              (meta (popup-item-symbol arg))
              (doc-buffer (company-doc-buffer (popup-item-documentation arg)))
              (annotation (popup-item-summary arg))
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

  (with-eval-after-load 'ivy
    (defun completion-in-region-company-or-ivy (start end collection &optional predicate)
      (if (eq (selected-window) (active-minibuffer-window))
          (ivy-completion-in-region start end collection predicate)
        (company-completion-in-region start end collection predicate))))

  (require 'company-quickhelp)
  (with-eval-after-load 'company-quickhelp
    (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
    (setq company-quickhelp-delay 0.1)
    (company-quickhelp-mode)))

(provide 'company-ext)
