;;;  -*- lexical-binding: t -*-

;; (require 'company)

(defun company-ext--remove-props (str &rest props)
  "Return STR with text PROPS destructively removed."
  (remove-list-of-text-properties 0 (length str) props str)
  str)

(setq company--return-fullpath t)
(defun completion-file-name-table-fullpath-hook (orig-fun string pred action)
  (if (and company--return-fullpath
           (eq action t))
      (let ((res (funcall orig-fun string pred action)))
        (-remove (lambda (x) (or (string= x "./") (string= x "../"))) res))
    (funcall orig-fun string pred action)))

(defun company-completion-in-region (start end collection &optional predicate)
  (lexical-let (
                (company-minimum-prefix-length 0)
                (company-idle-delay 0)
                (company-backend
                 (lambda (command &optional arg &rest ignored)
                   (interactive (list 'interactive))
                   (cl-case command
                     ;; (interactive (company-begin-backend 'fixed-candidate-source))
                     (prefix (buffer-substring-no-properties
                              (min start (buffer-end 1))
                              (min end (buffer-end 1))))
                     (candidates (let (
                                       (cands
                                        (let ((company--return-fullpath t))
                                          (completion-all-completions
                                           arg
                                           collection predicate
                                           (length arg)))
                                        )
                                       (rev-arg (string-reverse arg))
                                       )
                                   (unless (null cands)
                                     (setcdr (last cands) nil))
                                   (dolist (s cands)
                                     (company-ext--remove-props s 'face))
                                   cands))
                     (meta (if (stringp arg) (get-text-property 0 'symbol arg)))
                     (doc-buffer (company-doc-buffer
                                  (if (stringp arg) (get-text-property 0 'document arg))))
                     (annotation (if (stringp arg) (get-text-property 0 'summary arg)))
                     (location nil)
                     (no-cache t)
                     (sorted t))
                   ))
                )
    (company-complete-common)))


(defun company--async-continue (prefix ignore-case candidates)
  (when (company-call-backend 'no-cache company-prefix)
    ;; Don't complete existing candidates, fetch new ones.
    (setq company-candidates-cache nil))
  (let* ((new-prefix prefix)
         (c candidates))
    (cond
     ((company--unique-match-p c new-prefix ignore-case)
      ;; Handle it like completion was aborted, to differentiate from user
      ;; calling one of Company's commands to insert the candidate,
      ;; not to trigger template expansion, etc.
      (company-cancel 'unique))
     ((consp c)
      ;; incremental match
      (setq company-prefix new-prefix)
      (company-update-candidates c)
      c)
     ((and (characterp last-command-event)
           (company-auto-complete-p (string last-command-event)))
      ;; auto-complete
      (save-excursion
        (goto-char company-point)
        (let ((company--auto-completion t))
          (company-complete-selection))
        nil))
     ((not (company--incremental-p))
      (company-cancel))
     (t (company--continue-failed new-prefix)))))

(defun company--async-begin-new (prefix ignore-case candidates)
  (let ((c candidates))
    (cond
     ((and (company--unique-match-p c company-prefix ignore-case)
           (if company--manual-action
               ;; If `company-manual-begin' was called, the user
               ;; really wants something to happen.  Otherwise...
               (ignore (message "Sole completion"))
             t))
      ;; ...abort and run the hooks, e.g. to clear the cache.
      (company-cancel 'unique))
     ((null c)
      (when company--manual-action
        (message "No completion found")))
     (t ;; We got completions!
      (when company--manual-action
        (setq company--manual-prefix prefix))
      (company-update-candidates c)
      (if (and (not (cdr company-candidates))
               (equal company-common (car company-candidates)))
          (company-complete-selection)
        (company--insert-candidate company-common)
        (setq company-prefix company-common
              company-point (point)
              company--point-max (point-max)))
      (run-hook-with-args 'company-completion-started-hook
                          (company-explicit-action-p))
      (company-call-frontends 'show)))))

(defun company--async-perform (prefix ignore-case candidates)
  (if (not candidates)
      (setq company-backend nil
            company-candidates nil)
    (cond
     (company-candidates
      (company--async-continue prefix ignore-case candidates))
     ((company--should-complete)
      (company--async-begin-new prefix ignore-case candidates)))
    (company-ensure-emulation-alist)
    (company-enable-overriding-keymap company-active-map)
    (company-call-frontends 'update)))

(defun company--async-post-command (prefix ignore-case candidates)
  (condition-case-unless-debug err
      (progn
        (company--async-perform prefix ignore-case candidates)
        (when company-candidates
          (company-call-frontends 'post-command)))
    (error (message "Company: An error occurred in post-command")
           (message "%s" (error-message-string err))
           (company-cancel)))
  (company-install-map))

(defun company--async-calculate-candidates (prefix ignore-case candidates)
  (let ((candidates (company--preprocess-candidates candidates)))
    (push (cons prefix candidates) company-candidates-cache)
    (when candidates
      (setq candidates (company--postprocess-candidates candidates)))))

(defun company--fetch-candidates-async (prefix)
  (let* ((non-essential (not (company-explicit-action-p)))
         (inhibit-redisplay t)
         (c (if (or company-selection-changed
                    ;; FIXME: This is not ideal, but we have not managed to deal
                    ;; with these situations in a better way yet.
                    (company-require-match-p))
                (company-call-backend 'candidates prefix)
              (company-call-backend-raw 'candidates prefix))))
    (if (not (eq (car c) :async))
        c
      (funcall
       (cdr c)
       (lexical-let (
                     (prefix prefix)
                     (ignore-case (company-call-backend 'ignore-case))
                     (-company-point (point))
                     (-company--point-max (point-max))
                     (-company-backend company-backend)
                     (-company--manual-action company--manual-action)
                     (-company-previous-candidates company-candidates)
                     (buf (current-buffer))
                     (win (selected-window))
                     (tick (buffer-chars-modified-tick))
                     (pos (point))
                     )
         (lambda (candidates)
           (when (and (eq buf (current-buffer))
                      (eq win (selected-window))
                      (eq tick (buffer-chars-modified-tick))
                      (eq pos (point)))
             (setq company-prefix prefix
                   company-backend -company-backend
                   company--manual-action -company--manual-action
                   company-candidates -company-previous-candidates
                   company-point -company-point
                   company--point-max -company--point-max)
             (let ((this-command 'company-complete-common)
                   (company-idle-delay 'now))
               (company--async-calculate-candidates prefix ignore-case candidates)
               (company--async-post-command prefix ignore-case candidates))))))
      (setq company--manual-action nil)
      nil)))

(with-eval-after-load 'company
  (add-to-list 'company-continue-commands 'comint-previous-matching-input-from-input t)
  (add-to-list 'company-continue-commands 'comint-next-matching-input-from-input t)
  (advice-add 'completion-file-name-table :around #'completion-file-name-table-fullpath-hook)
  (defalias 'company--fetch-candidates 'company--fetch-candidates-async)
  (setq company-idle-delay nil)
  (setq company-require-match nil)

  (setq company-orig--completion-in-region-function
        'completion--in-region)
  (defun completion-in-region-company-or-ivy (start end collection &optional predicate)
    (if (or (not company-mode)
            (eq (selected-window) (active-minibuffer-window)))
        (funcall company-orig--completion-in-region-function
                 start end collection predicate)
      (company-completion-in-region start end collection predicate)))

  (define-key company-active-map (kbd "C-i") (lambda()
                                               (interactive)
                                               (let ((completion-in-region-function company-orig--completion-in-region-function))
                                                 (completion-at-point))))

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
                    'completion-in-region-company-or-ivy)
              (global-company-mode))))

(provide 'company-ext)
