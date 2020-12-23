;; (require 'ivy)
;; (require 'shell-ext)
;; (require 'python-ext)
;; (require 'utils)

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

(defun find-file-convert-to-ag ()
  (interactive)
  (ivy-quit-and-run
    (counsel-ag-preselect ivy--directory)))

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

(defun counsel-find-file-occur-hook(orig-fun &rest args)
  (let (
        (res (apply orig-fun args))
        (caller (ivy-state-caller ivy-last))
        )
    (rename-buffer
     (if (eq caller 'counsel-find-file)
         (format "*files: %s%s*"
                 ivy--directory
                 ivy-text)
       (format "*ivy-occur%s \"%s\" dir: %s*"
             (if caller
                 (concat " " (prin1-to-string caller))
               "")
             ivy-text
             ivy--directory)) t)
     res))

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



(defun ivy--handle-directory-hook (orig-fun input)
  (or (funcall orig-fun input)
      (when (and (string-match-p "\\`/\\([^/]+:\\)*\\'" ivy--directory)
                 (string-match-p "\\`[^/]+\:\\'" (ivy-state-current ivy-last)))
        (setq ivy-text (ivy-state-current ivy-last))
        (concat ivy--directory ivy-text))))


(defun ivy--parent-dir (filename)
  "Return parent directory of absolute FILENAME."
  (let (
        (curdirname (directory-file-name filename))
        )
  (if (string-match "\\`\\(/\\([^/]+:\\)*\\)[^/]+:/?\\'" curdirname)
      (match-string-no-properties 1 curdirname)
    (file-name-directory curdirname))))


(defcustom ivy-magic-root t
    "When non-nil, / will move to root when selecting files.
Otherwise, // will move to root."
    :type 'boolean)
(defun ivy--magic-file-slash-hook (orig-fun &rest args)
  (when (and (string= "/" ivy-text) ivy-magic-root)
    (setq ivy-text "//"))
  (if (string= (concat ivy--directory ivy-text) "/root:")
    (setq ivy-text "/sudo:root@localhost:")
    (when (string-match "\\`/[^\:]+:[^\:]+:\\'" (concat ivy--directory ivy-text))
      (setq ivy-text (concat ivy--directory ivy-text))))
  (apply orig-fun args))

(defun ivy--magic-tilde-directory-hook (orig-fun dir)
  (expand-file-name
   (or (file-remote-p ivy--directory)
       (funcall orig-fun dir))))

(defun ivy-set-text (str)
  "Set `ivy-text' to STR."
  (setq ivy-text str)
  (setq ivy-regex (funcall ivy--regex-function ivy-text)))

(defun ivy--exhibit ()
  "Insert Ivy completions display.
Should be run via minibuffer `post-command-hook'."
  (when (memq 'ivy--queue-exhibit post-command-hook)
    (let ((inhibit-field-text-motion nil))
      (constrain-to-field nil (point-max)))
    (ivy-set-text (ivy--input))
    (let ((new-minibuffer (ivy--update-minibuffer)))
      (when new-minibuffer
        (ivy--insert-minibuffer new-minibuffer)))
    t))

(defun ivy--dynamic-collection-cands (input)
  (let ((coll (funcall (ivy-state-collection ivy-last) input)))
    (if (listp coll)
        (mapcar (lambda (x) (if (consp x) (car x) x)) coll)
      coll)))

(defun ivy--update-minibuffer ()
  (prog1
      (if (ivy-state-dynamic-collection ivy-last)
          ;; while-no-input would cause annoying
          ;; "Waiting for process to die...done" message interruptions
          (let ((inhibit-message t)
                coll in-progress)
            (unless (or (equal ivy--old-text ivy-text)
                        (eq this-command 'ivy-resume))
              (while-no-input
                (setq coll (ivy--dynamic-collection-cands ivy-text))
                (when (eq coll 0)
                  (setq coll nil)
                  (setq ivy--old-re nil)
                  (setq in-progress t))
                (setq ivy--all-candidates (ivy--sort-maybe coll))))
            (when (eq ivy--all-candidates 0)
              (setq ivy--all-candidates nil)
              (setq ivy--old-re nil)
              (setq in-progress t))
            (when (or ivy--all-candidates
                      (and (not (get-process " *counsel*"))
                           (not in-progress)))
              (ivy--set-index-dynamic-collection)
              (ivy--format ivy--all-candidates)))
        (cond (ivy--directory
               (cond ((or (string= "~/" ivy-text)
                          (and (string= "~" ivy-text)
                               ivy-magic-tilde))
                      (ivy--cd (ivy--magic-tilde-directory ivy--directory)))
                     ((or (string-match "/\\'" ivy-text)
                          (and (string-match-p "\\`/\\([^/]+:\\)*\\'" ivy--directory)
                               (string-match-p "\\`[^/]+\:\\'" ivy-text)))
                      (ivy--magic-file-slash)
                      (setq ivy--directory (expand-file-name ivy--directory))
                      (ivy--insert-prompt))))
              ((eq (ivy-state-collection ivy-last) #'internal-complete-buffer)
               (when (or (and (string-match "\\` " ivy-text)
                              (not (string-match "\\` " ivy--old-text)))
                         (and (string-match "\\` " ivy--old-text)
                              (not (string-match "\\` " ivy-text))))
                 (setq ivy--all-candidates
                       (if (= (string-to-char ivy-text) ?\s)
                           (ivy--buffer-list " ")
                         (ivy--buffer-list "" ivy-use-virtual-buffers)))
                 (setq ivy--old-re nil))))
        (with-current-buffer (ivy-state-buffer ivy-last)
          (ivy--format
           (ivy--filter ivy-text ivy--all-candidates))))
    (setq ivy--old-text ivy-text)))

(with-eval-after-load 'ivy
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-magic-tilde nil)
  ;; (icomplete-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-wrap t)
  (setq ivy-auto-select-single-candidate t)
  (setq ivy-on-del-error-function 'ignore)
  (setq ivy-magic-slash-non-match-action nil)

  (advice-add 'counsel-find-file-occur :around #'counsel-find-file-occur-hook)
  (advice-add 'ivy--magic-file-slash :around #'ivy--magic-file-slash-hook)
  (advice-add 'ivy--handle-directory :around #'ivy--handle-directory-hook)
  (advice-add 'ivy--magic-tilde-directory :around #'ivy--magic-tilde-directory-hook)

  (advice-add 'ivy-mouse-offset :around #'ivy--mouse-hook)

  (advice-add 'ivy-next-history-element :around #'ignore-errors-hook)
  (advice-add 'ivy-previous-history-element :around #'ignore-errors-hook))

(provide 'ivy-utils)
