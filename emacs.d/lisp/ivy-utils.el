(require 'ivy)
(require 'shell-utils)
; (require 'python-utils)


; (with-current-buffer buffer
;   )
; (ivy-exit-with-action
;  (lambda (_) (pop-to-buffer buffer)))

(defun ivy-shell ()
  (interactive)
  (if ivy--directory
    (ivy-quit-and-run
        (switch-to-buffer (old-shell-with-dir (expand-file-name ivy--directory))))
    (user-error
     "Not completing files currently")))

(defun ivy-new-shell ()
  (interactive)
  (if ivy--directory
      (ivy-quit-and-run
        (switch-to-buffer (new-shell-with-dir (expand-file-name ivy--directory))))
    (user-error
     "Not completing files currently")))

(defun ivy-magit-status ()
  (interactive)
  (if ivy--directory
      (ivy-quit-and-run
       (magit-status ivy--directory))
    (user-error
     "Not completing files currently")))

(defun ivy-python ()
  (interactive)
  (if ivy--directory
      (ivy-quit-and-run
       (new-python-in-dir ivy--directory))
    (user-error
     "Not completing files currently")))

(defun swiper-comment-or-uncomment-line ()
  (interactive)
  (with-ivy-window
    (my-comment-or-uncomment-region)
    (ivy--exhibit)))

(defun swiper-kill-line ()
  (interactive)
  (with-ivy-window
    (kill-whole-line)
    (ivy--exhibit)))

(defun swiper-convert-to-ag ()
  (interactive)
  (ivy-quit-and-run
    (counsel-ag-preselect ivy-text)))

(defun swiper--goto-original-point()
  (interactive)
  (with-ivy-window
    (setq swiper--current-match-start swiper--opoint)
    (setq swiper--current-line (string-to-number (format-mode-line "%l")))
    (goto-char swiper--opoint)))

(provide 'ivy-utils)
