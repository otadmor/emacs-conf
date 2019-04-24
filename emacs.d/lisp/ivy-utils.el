(require 'ivy)
(require 'shell-ext)
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

(defun ivy-find-file-as-root ()
  (interactive)
  (counsel-find-file-as-root ivy-text))

(defun ivy-previous-line-or-history-2 (arg)
  "Move cursor vertically up ARG candidates.
If the input is empty, select the previous history element instead."
  (interactive "p")
  (when (string= ivy-text "")
    (ivy-previous-history-element 1))
  (ivy-previous-line arg))

(defun counsel-find-file-occur-hook()
  (let (
        (caller (ivy-state-caller ivy-last))
        )
    (rename-buffer
     (format "*ivy-occur%s \"%s\" dir: %s*"
             (if caller
                 (concat " " (prin1-to-string caller))
               "")
             ivy-text
             ivy--directory)
     t)))
(advice-add 'counsel-find-file-occur :after #'counsel-find-file-occur-hook)

(provide 'ivy-utils)
