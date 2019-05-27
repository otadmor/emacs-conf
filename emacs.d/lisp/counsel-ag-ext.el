(require 'ivy)
(require 'counsel)
(require 'dumb-jump)
; (push '(counsel-ag . ivy-recompute-index-swiper-async) ivy-index-functions-alist)

(defun ag--format-result (proj result)
  (let (
        (rep-path (plist-get result :path))
        )
    (format "%s:%s:%s"
            (if (null rep-path) proj (s-replace proj "" (plist-get result :path)))
            (plist-get result :line)
            (plist-get result :context))))


(defun preselect-line(proj)
  (with-selected-window (if (active-minibuffer-window)
                            (if (eq (active-minibuffer-window) (frame-selected-window))
                                (get-mru-window)
                              (frame-selected-window))
                          (frame-selected-window))
    (let (
          (text (thing-at-point 'line t))
          )
      (unless (null text)
        (let (
              (pselect-record (list :path buffer-file-name
                                    :line (string-to-number
                                           (format-mode-line "%l"))
                                    :context (replace-regexp-in-string
                                              "\n$" "" text)))
              )
          (ag--format-result (expand-file-name proj) pselect-record))))))


(defun counsel-git-grep-action-hook (orig-fun &rest args)
  (let (
        (x (car args))
        )
    (if (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
      (let* (
             (file-name (match-string-no-properties 1 x))
             (file-attributes (file-attributes file-name))
             (file-size (nth 7 file-attributes))
             )
        (when (or (<= file-size large-file-warning-threshold)
                  (eq this-command 'ivy-alt-done)
                  (eq this-command 'ivy-done))
          (apply orig-fun args)))
      (apply orig-fun args))))
(advice-add 'counsel-git-grep-action :around #'counsel-git-grep-action-hook)

(defun counsel-ag-preselect (&optional initial-input initial-directory extra-ag-args ag-prompt)
  "Grep for a string in the current directory using ag.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS string, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument."
  (interactive)
  (setq counsel-ag-command counsel-ag-base-command)
  (counsel-require-program (car (split-string counsel-ag-command)))
  (when current-prefix-arg
    (setq initial-directory
          (or initial-directory
              (read-directory-name (concat
                                    (car (split-string counsel-ag-command))
                                    " in directory: "))))
    (setq extra-ag-args
          (or extra-ag-args
              (read-from-minibuffer (format
                                     "%s args: "
                                     (car (split-string counsel-ag-command)))))))
  (setq counsel-ag-command (counsel--format-ag-command (or extra-ag-args "") "%s"))
  (let ((default-directory (or initial-directory
                               (locate-dominating-file default-directory ".git")
                               default-directory)))
    (let (
          (preselect (preselect-line default-directory))
          )
      (ivy-read (or ag-prompt
                    (concat (car (split-string counsel-ag-command)) ": "))
                #'counsel-ag-function
                :initial-input initial-input
                :preselect preselect
                :dynamic-collection t
                :keymap counsel-ag-map
                :history 'counsel-git-grep-history
                :action #'counsel-git-grep-action
                :unwind (lambda ()
                          (counsel-delete-process)
                          (swiper--cleanup))
                :caller 'counsel-ag))))

(defun counselag-which-func-update (x)
  (swiper--async-which-func-update))
(advice-add 'counsel-git-grep-action :after #'counselag-which-func-update)

(provide 'counsel-ag-ext)
