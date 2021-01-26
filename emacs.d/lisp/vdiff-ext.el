;; -*- lexical-binding: t; -*-

(with-eval-after-load 'vdiff
  (setq vdiff-lock-scrolling t)
 (setq vdiff-disable-folding t)
 (setq vdiff-truncate-lines t)

 (define-key vdiff-mode-map (kbd "C-c") 'vdiff-hydra/body)
 (define-key vdiff-mode-map (kbd "C-n") 'vdiff-next-hunk)
 (define-key vdiff-mode-map (kbd "C-p") 'vdiff-previous-hunk)

 (define-key vdiff-3way-mode-map (kbd "C-c") 'vdiff-hydra/body)
 (define-key vdiff-3way-mode-map (kbd "C-n") 'vdiff-next-hunk)
 (define-key vdiff-3way-mode-map (kbd "C-p") 'vdiff-previous-hunk)

 (set-transient-map 'vdiff-mode-map)
 (set-transient-map 'vdiff-3way-mode-map)

 (setq vdiff-default-refinement-syntax-code "w_")
 (setq vdiff-auto-refine t)

 (setq vdiff--after-change-refresh-delay 0.01)
 (defun vdiff() (interactive) (call-interactively 'vdiff-files)))

(provide 'vdiff-ext)
