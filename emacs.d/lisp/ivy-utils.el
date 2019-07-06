(require 'ivy)
(require 'shell-ext)
(require 'python-ext)
(require 'utils)

(setq ivy-format-function #'ivy-format-function-line)
(setq ivy-magic-tilde nil)
;(icomplete-mode t)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "%d/%d ")

(setq ivy-wrap t)
(setq ivy-auto-select-single-candidate t)
(setq ivy-on-del-error-function 'ignore)
;(setq ivy-magic-slash-non-match-action nil)

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


(defun ivy--parent-dir (filename)
  "Return parent directory of absolute FILENAME."
  (let (
        (curdirname (directory-file-name filename))
        )
  (if (string-match "\\`\\(/\\([^/]+:\\)*\\)[^/]+:/?\\'" curdirname)
      (match-string-no-properties 1 curdirname)
    (file-name-directory curdirname))))

(defun ivy--directory-done ()
  "Handle exit from the minibuffer when completing file names."
  (let (dir)
    (cond
      ((equal ivy-text "/sudo::")
       (setq dir (concat ivy-text (expand-file-name ivy--directory)))
       (ivy--cd dir)
       (ivy--exhibit))
      ((ivy--directory-enter))
      ((unless (string= ivy-text "")
         (let ((file (expand-file-name
                      (if (> ivy--length 0) (ivy-state-current ivy-last) ivy-text)
                      ivy--directory)))
           (when (ignore-errors (file-exists-p file))
             (if (file-directory-p file)
                 (ivy--cd (file-name-as-directory file))
               (ivy-done))
             ivy-text))))
      ((or (and (equal ivy--directory "/")
                (string-match-p "\\`[^/]+:.*:.*\\'" ivy-text))
           (string-match-p "\\`/[^/]+:.*:.*\\'" ivy-text))
       (ivy-done))
      ((or (and (equal ivy--directory "/")
                (cond ((string-match
                        "\\`\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
                        ivy-text)
                       (setq ivy-text (ivy-state-current ivy-last)))
                      ((string-match
                        "\\`\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
                        (ivy-state-current ivy-last))
                       (setq ivy-text (ivy-state-current ivy-last)))))
           (string-match
            "\\`/\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
            ivy-text))
       (let ((method (match-string 1 ivy-text))
             (user (match-string 2 ivy-text))
             (rest (match-string 3 ivy-text))
             res)
         (require 'tramp)
         (dolist (x (tramp-get-completion-function method))
           (setq res (append res (funcall (car x) (cadr x)))))
         (setq res (delq nil res))
         (when user
           (dolist (x res)
             (setcar x user)))
         (setq res (delete-dups res))
         (let* ((old-ivy-last ivy-last)
                (enable-recursive-minibuffers t)
                (host (let ((ivy-auto-select-single-candidate nil))
                        (setq ivy--directory (concat "/" method ":"))
                        (ivy-read (concat "  Find File: /" method ":")
                                  (mapcar #'ivy-build-tramp-name res)
                                  :initial-input rest))))
           (setq ivy-last old-ivy-last)
           (setq ivy--prompt "  Find File: ")
           (when host
             (setq ivy--directory "/")
             (ivy--cd (concat "/" method ":" host ":"))
             (setq ivy--directory (expand-file-name ivy--directory)))
           (ivy--insert-prompt))))
      ((and (string-match-p "\\`/\\([^/]+:\\)*\\'" ivy--directory)
            (string-match-p "\\`[^/]+\:\\'" (ivy-state-current ivy-last)))
       (setq ivy-text (ivy-state-current ivy-last))
       (ivy--cd (concat ivy--directory ivy-text))
       (setq ivy--directory (expand-file-name ivy--directory))
       (ivy--insert-prompt))
      (t
       (ivy-done)))))


(defun ivy--exhibit ()
  "Insert Ivy completions display.
Should be run via minibuffer `post-command-hook'."
  (when (memq 'ivy--queue-exhibit post-command-hook)
    (let ((inhibit-field-text-motion nil))
      (constrain-to-field nil (point-max)))
    (setq ivy-text (ivy--input))
    (if (ivy-state-dynamic-collection ivy-last)
        ;; while-no-input would cause annoying
        ;; "Waiting for process to die...done" message interruptions
        (let ((inhibit-message t))
          (unless (equal ivy--old-text ivy-text)
            (while-no-input
              (setq ivy--all-candidates
                    (ivy--sort-maybe
                     (funcall (ivy-state-collection ivy-last) ivy-text)))
              (setq ivy--old-text ivy-text)))
          (when (or ivy--all-candidates
                    (not (get-process " *counsel*")))
            (ivy--insert-minibuffer
             (ivy--format ivy--all-candidates))))
      (cond (ivy--directory
             (cond ((or (string= "~/" ivy-text)
                        (and (string= "~" ivy-text)
                             ivy-magic-tilde))
                    (ivy--cd
                     (expand-file-name
                      (if (string-match "\\`\\(/\\([^/]+:\\)+\\)/" ivy--directory)
                          (match-string 1 ivy--directory)
                        "~/"))))
                   ((string-match "/\\'" ivy-text)
                    (ivy--magic-file-slash)
                    (setq ivy--directory (expand-file-name ivy--directory))
                    (ivy--insert-prompt))
                   ((and (string-match-p "\\`/\\([^/]+:\\)*\\'" ivy--directory)
                         (string-match-p "\\`[^/]+\:\\'" ivy-text))
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
      (ivy--insert-minibuffer
       (with-current-buffer (ivy-state-buffer ivy-last)
         (ivy--format
          (ivy--filter ivy-text ivy--all-candidates))))
      (setq ivy--old-text ivy-text))))

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
(advice-add 'ivy--magic-file-slash :around #'ivy--magic-file-slash-hook)




(defun ivy-backward-delete-char ()
  "Forward to `delete-backward-char'.
Call `ivy-on-del-error-function' if an error occurs, usually when
there is no more text to delete at the beginning of the
minibuffer."
  (interactive)
  (if (and ivy--directory (= (minibuffer-prompt-end) (point)))
      (progn
        (ivy--cd (ivy--parent-dir (expand-file-name ivy--directory)))
        (ivy--exhibit))
    (setq prefix-arg current-prefix-arg)
    (condition-case nil
        (call-interactively #'delete-backward-char)
      (error
       (when ivy-on-del-error-function
         (funcall ivy-on-del-error-function))))))

(defun ivy--sorted-files (dir)
  "Return the list of files in DIR.
Directories come first."
  (let* ((default-directory dir)
         (seq (condition-case nil
                  (all-completions "" #'read-file-name-internal
                                   (ivy-state-predicate ivy-last))
                (error
                 (directory-files dir))))
         sort-fn)
    (setq seq (delete-dups seq))
    (setq seq (delete "./" (delete "../" seq)))
    (when (eq (setq sort-fn (ivy--sort-function #'read-file-name-internal))
              #'ivy-sort-file-function-default)
      (setq seq (mapcar (lambda (x)
                          (propertize x 'dirp (ivy--dirname-p x)))
                        seq)))
    (when sort-fn
      (setq seq (sort seq sort-fn)))
    (dolist (dir ivy-extra-directories)
      (push dir seq))
    (if (string= dir "/")
        (cl-remove-if (lambda (s) (string-match ":$" s)) (delete "../" seq))
      seq)))

(defun ivy--magic-file-slash ()
  "Handle slash when completing file names."
  (when (or (and (eq this-command #'self-insert-command)
                 (eolp))
            (eq this-command #'ivy-partial-or-done))
    (let ((canonical (expand-file-name ivy-text ivy--directory))
          (magic (not (string= ivy-text "/"))))
      (cond ((member ivy-text ivy--all-candidates)
             (ivy--cd canonical))
            ((string-match-p "//\\'" ivy-text)
             (ivy--cd (if (string-match "\\`/\\([^/]+:\\)+/" ivy--directory)
                          (match-string 0 ivy--directory)
                        "/")))
            ((string-match-p "\\`/ssh:" ivy-text)
             (ivy--cd (file-name-directory ivy-text)))
            ((string-match "[[:alpha:]]:/\\'" ivy-text)
             (let ((drive-root (match-string 0 ivy-text)))
               (when (file-exists-p drive-root)
                 (ivy--cd drive-root))))
            ((and magic (file-directory-p canonical))
             (ivy--cd canonical))
            ((let ((default-directory ivy--directory))
               (and (or (> ivy--index 0)
                        (= ivy--length 1)
                        magic)
                    (not (equal (ivy-state-current ivy-last) ""))
                    (file-directory-p (ivy-state-current ivy-last))
                    (or (eq ivy-magic-slash-non-match-action
                            'ivy-magic-slash-non-match-cd-selected)
                        (eq this-command #'ivy-partial-or-done))))
             (ivy--cd
              (expand-file-name (ivy-state-current ivy-last) ivy--directory)))
            ((and (eq ivy-magic-slash-non-match-action
                      'ivy-magic-slash-non-match-create)
                  magic)
             (ivy--create-and-cd canonical))))))

(provide 'ivy-utils)
