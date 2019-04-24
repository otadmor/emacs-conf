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

(provide 'ivy-utils)
