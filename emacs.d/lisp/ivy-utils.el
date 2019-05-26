(require 'ivy)
(require 'shell-ext)
(require 'python-ext)
(require 'utils)

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

(defun ivy--mouse-hook (orig-fun &rest args)
  (let* (
         (event (car args))
         )
    (when event
      (let* (
             (event-start (event-start event))
             (event-column (car (posn-actual-col-row event-start)))
             (point (posn-point event-start))
             (line-len (progn
                         (save-excursion
                           (goto-char point)
                           (let* (
                                  (line-begin (line-beginning-position))
                                  (line-end (line-end-position))
                                  )
                             (- line-end line-begin)))))
             )
        (if (< event-column line-len)
            (apply orig-fun args)
          (select-window (active-minibuffer-window))
          nil)))))
(advice-add 'ivy-mouse-offset :around #'ivy--mouse-hook)

(advice-add 'ivy-next-history-element :around #'ignore-errors-hook)
(advice-add 'ivy-previous-history-element :around #'ignore-errors-hook)

(defcustom ivy-magic-root t
  "When non-nil, / will move to root when selecting files.
Otherwise, // will move to root."
  :type 'boolean)
(defun ivy--magic-file-slash-hook (orig-fun &rest args)
  (when (and (string-match-p "^/$" ivy-text) ivy-magic-root)
    (setq ivy-text "//"))
  (apply orig-fun args))
(advice-add 'ivy--magic-file-slash :around #'ivy--magic-file-slash-hook)

(provide 'ivy-utils)
