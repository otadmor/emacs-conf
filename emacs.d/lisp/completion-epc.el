;;;  -*- lexical-binding: t -*-
(require 'epc)
(require 'shell)
(require 'comint)
(require 'epcs)

; (defvar complete-server-regexp
;   (concat "^" (regexp-quote "___EPCCompletionServer_PORT=") "\\([[:xdigit:]]+\\)\n$")
;   "How the port of `completion' the server is shown to the screen ")

(defvar complete-server-regexp
  (concat (regexp-quote "___EPCCompletionServer_PORT=") "\\([[:xdigit:]]+\\)\n")
  "How the port of `completion' the server is shown to the screen ")


(setq mngr-complete-epc nil)
(make-local-variable 'mngr-complete-epc)
(defun completion-epc-kill-mngr (mngr)
  (unless (null mngr)
    (when (eq mngr-complete-epc mngr)
      (setq-local mngr-complete-epc nil)
      (kill-local-variable 'mngr-complete-epc))))

(defun completion-epc-clear-completion-epc-hook(orig-fun &rest args)
  (let (
        (mngr (car args))
        )
    (completion-epc-kill-mngr mngr)
    (apply orig-fun args)))
(advice-add 'epc:stop-epc :around #'completion-epc-clear-completion-epc-hook)

(defun epc:with-manager-for-connection (connection func)
  (loop
   for mngr in epc:live-connections collect
   (list (when (eq (epc:manager-connection mngr) connection )
           (funcall func mngr)))))

(defun epc:process-filter-stop-on-error(orig-fun &rest args)
  (condition-case err
      (apply orig-fun args)
    (error
     (let (
           (connection (car args))
           )
       (unless (null connection)
         (epc:with-manager-for-connection
          connection
          (lambda (mngr) (epc:stop-epc mngr))))))))
(advice-add 'epc:process-filter :around #'epc:process-filter-stop-on-error)


(defun epc:process-available-input-check-buffer(orig-fun &rest args)
  (when (buffer-live-p (process-buffer process))
    (apply orig-fun args)))
(advice-add 'epc:process-available-input :around #'epc:process-available-input-check-buffer)


(defun epc:connect-remote-server (server-address server-port)
  (let (
        (mngr (make-epc:manager
              :connection (epc:connect server-address server-port)
              ))
        )
    (epc:init-epc-layer mngr)
    mngr))


(defun epc:net-read-or-lose-fix (process)
  (epc:net-read))
(defalias 'epc:net-read-or-lose 'epc:net-read-or-lose-fix)


(defun epc:net-read-fix ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (epc:net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length)) content)
    (if (plusp length)
      (prog1 (save-restriction
               (narrow-to-region start end)
               (read (decode-coding-string
                      (buffer-string) 'utf-8-unix)))
        (delete-region (point-min) end))
      (signal error "(plusp length)")
      nil)))
(defalias 'epc:net-read 'epc:net-read-fix)


(defun connect-completion-server (server-port)
  (message "Found EPC server at %S" server-port)
  (let (
        (mngr (epc:connect-remote-server
               "localhost"
               server-port))
        )
    (setq-local mngr-complete-epc mngr)))


(defun epcs:server-start-with-host-reuseaddr-nowait (connect-function &optional port host)
  "Start TCP Server and return the main process object."
  (lexical-let*
      ((connect-function connect-function)
       (name (format "EPC Server %s" (epc:uid)))
       (buf (epc:make-procbuf (format "*%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4 :server t
         ;; :nowait t
         :host (or host 'local) :service (or port t) ; "127.0.0.1"
         :reuseaddr t
         :sentinel
         (lambda (process message)
           (epcs:sentinel process message connect-function)))))
    ;; (unless port
    ;;   ;; notify port number to the parent process via STDOUT.
    ;;   (message "%s\n" (process-contact main-process :service)))
    (push (cons main-process
                (make-epcs:server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          epcs:server-processes)
    main-process))

(defun epc:start-server-and-set-env ()
  (let* (
         (working-buffer (current-buffer))
         (epc-server-process
          (epcs:server-start-with-host-reuseaddr-nowait
           (lambda (mngr)
             (epc:incoming-connection-made-mngr mngr working-buffer))))
         (port (cadr (process-contact epc-server-process)))
         )
    (with-current-buffer working-buffer
      (add-hook 'kill-buffer-hook (lambda ()
                                    (epcs:server-stop epc-server-process)) t t))
    (message "EPC Server port %S for buffer %S" port (current-buffer))
    (make-local-variable 'process-environment)
    (setenv "EPC_COMPLETION_SERVER_PORT" (format "%d" port))))


(defun epc:incoming-connection-made-mngr (mngr working-buffer)
  (with-current-buffer working-buffer
    (message "EPC client connected in buffer %S" (current-buffer))
    (setq-local mngr-complete-epc mngr)))

(defun completion--comint-output-filter (string)
  (save-match-data
    (when (not
           (let (
                 (start (marker-position comint-last-input-end))
                 (end (if (boundp 'comint-last-prompt-overlay)
                          (and comint-last-prompt-overlay
                               (overlay-start comint-last-prompt-overlay))
                        (and comint-last-prompt (cdr comint-last-prompt))))
                 )
             (when (and start end (< start end))
               (let (
                     (new-output-chunk (buffer-substring-no-properties start end))
                     )
                 (when (string-match complete-server-regexp new-output-chunk)
                     (when server-port
                       (delete-region start end))))))))
    (when (string-match complete-server-regexp string)
      (let (
            (server-port   (match-string 1 string))
            )
        (when server-port
          (connect-completion-server server-port)
          (concat
           (substring-no-properties string 0 (match-beginning 0))
           (substring-no-properties string (match-end 0) nil)))))))

(add-hook 'comint-output-filter-functions 'completion--comint-output-filter nil nil)
(add-hook 'comint-mode-hook 'epc:start-server-and-set-env)

(defun epc-complete-deferred-mngr (mngr to-complete)
  (when mngr
    (condition-case nil
        (epc:call-deferred mngr 'complete to-complete)
      (error (progn
               ;; (epc:stop-epc mngr)
               (message "error in completion server")
               (completion-epc-kill-mngr mngr)
               (deferred:next (lambda () '())))))))

(defun epc-complete-deferred (to-complete)
  (epc-complete-deferred-mngr mngr-complete-epc to-complete))

(defun completion-epc-collect-candidates (completion)
  "Return a candidate from a COMPLETION reply."
  (if (string-or-null-p completion)
      completion
    (let ((candidate (plist-get completion :word)))
      (when candidate
        (put-text-property 0 1 :doc (plist-get completion :doc) candidate)
        (put-text-property 0 1 :symbol (plist-get completion :symbol) candidate)
        (put-text-property 0 1 :description (plist-get completion :description) candidate)
        candidate))))


(defun completion-epc-complete-deferred(prefix)
  (cons :async
        (lambda (callback)
          (deferred:$
            (epc-complete-deferred prefix)
            (deferred:nextc it
              (lambda (reply)
                (let ((candidates
                       (mapcar 'completion-epc-collect-candidates reply)))
                    (funcall callback candidates))))))))

; (defun epc-completion-at-point ()
;   (when (comint--match-partial-filename)
;     (let (
;           (start (nth 0 (match-data)))
;           (end (nth 1 (match-data)))
;           )
;       (when (and start end (< start end))
;         (let (
;               (to-complete (buffer-substring-no-properties start end))
;               )
;           (let (
;                 ;;; TODO - for some reason epc-complete runs twice,
;                 ;; which is very expensive.
;                 (completions (epc-complete to-complete))
;                 )
;             (when completions
;               (list start end completions ())
;               )))))))

; hook both, just in case it runs without or with shell
; (add-hook 'shell-dynamic-complete-functions 'epc-completion-at-point nil nil)
; (add-hook 'comint-dynamic-complete-functions 'epc-completion-at-point nil nil)

(require 'cl-lib)
(require 'company)
(require 'auto-complete)

(defun epc-completion-add-company(completion-mode hook prefix-cb)
  (unless (null hook)
    (let (
          (completion-func
           (lambda (command &optional arg &rest ignored)
             (interactive (list 'interactive))
             (cl-case command
               (interactive (company-begin-backend 'completion-func))
               (prefix (funcall prefix-cb))
               (candidates (completion-epc-complete-deferred arg))
               (meta (get-text-property 0 :description arg))
               (doc-buffer (company-doc-buffer (get-text-property 0 :doc arg)))
               (annotation (format "[%s]" (get-text-property 0 :symbol arg)))
               (location nil)
               (sorted t))
             ))
          )
      (eval-after-load 'company
        (lambda ()
          (add-hook hook
                    (lambda ()
                      (add-to-list 'company-backends completion-func))))))))

(defun epc-completion-add-auto-complete (completion-mode hook prefix-cb)
  (unless (null completion-mode)
    (add-to-list 'ac-modes completion-mode))
  (unless (null hook)
    (add-hook
     hook
     (lambda ()
       (let (
             (ac-epc-complete-reply nil)
             (ac-epc-complete-deferred nil)
             (ac-epc-received-response nil)
             (working-buffer (current-buffer))
             )
         (let* (

                (ac-epc-matches
                 (lambda ()
                   (with-current-buffer working-buffer
                     (unless ac-epc-received-response
                       (deferred:sync! ac-epc-complete-deferred))
                     (setq ac-epc-complete-deferred nil)
                     (unless (null ac-epc-complete-reply)
                       ;; should we creat ea timer and wait for responses
                       (cl-remove-if
                        'null
                        (mapcar
                         (lambda (x)
                           (unless (null x)
                             (if (stringp x)
                                 (popup-make-item x)
                               (condition-case nil
                                   (let (
                                         (word (plist-get x :word))
                                         (doc (plist-get x :doc))
                                         (description (plist-get x :description))
                                         (symbol (plist-get x :symbol))
                                         (pos (plist-get x :pos))
                                         )
                                     (let (
                                           (i (popup-make-item
                                               word
                                               :symbol symbol
                                               :document
                                               (unless (equal doc "") doc)
                                               :summary description))
                                           )
                                       (put-text-property 0 1 :pos pos i)
                                       i))
                                 (error nil)))))
                         ac-epc-complete-reply))))))

                (ac-epc-complete-request
                 (lambda (&optional prefix)
                   (with-current-buffer working-buffer
                     (let (
                           (prefix (or prefix (funcall prefix-cb)))
                           )
                       (unless (null ac-epc-complete-deferred)
                         (deferred:cancel ac-epc-complete-deferred))
                       (setq ac-epc-complete-reply nil)
                       (setq ac-epc-received-response nil)
                       (setq ac-epc-complete-deferred
                             (deferred:$
                               (deferred:next
                                 (lambda ()
                                   (with-current-buffer working-buffer
                                     (epc-complete-deferred prefix))))
                               (deferred:nextc it
                                 (lambda (reply)
                                   (with-current-buffer working-buffer
                                     (setq ac-epc-complete-reply reply)
                                     (setq ac-epc-received-response t))))
                               (deferred:error it
                                 (lambda (err)
                                   (cond
                                    ((stringp err)
                                     ;; application error
                                     ;; err: error message
                                     (message "error completion epc %S" err)
                                     )
                                    ((eq 'epc-error (car err))
                                     ;; epc error
                                     ;; err: (cadr err) -> error information
                                     (message "error completion epc2 %S" (cadr err))))))))))))

                (ac-completion-prefix
                 (lambda ()
                   (with-current-buffer working-buffer
                     (let (
                           (prefix (funcall prefix-cb))
                           )
                       (when (stringp prefix)
                         (- (point) (length prefix)))))))

                (ac-epc-source
                 (list
                  (list 'requires -1)
                  (list 'init ac-epc-complete-request)
                  (list 'candidates ac-epc-matches)
                  (list 'prefix ac-completion-prefix)))

                (ac-completion-func
                 (cl-defun acf (&key (expend ac-expand-on-auto-complete))
                   (interactive)
                   (let (
                         (prefix (funcall prefix-cb))
                         )
                     (when (and (stringp prefix) (> (length prefix) 0))
                       (deferred:nextc (funcall ac-epc-complete-request)
                         (lambda ()
                           (let ((ac-expand-on-auto-complete expand))
                             (ac-start :triggered 'command))))))))

                (old-completion-at-point-functions
                 (copy-sequence completion-at-point-functions))

                ;; see https://github.com/jwiegley/emacs-release/blob/master/lisp/minibuffer.el
                ;; completion-at-point
                ;;     - runs each of the completion-at-point-functions
                ;;         - if a result was found, then call completion-at-region
                ;;             - completion-at-region will call completion-all-completions to get
                ;;               the final completion list (the completion-at-point-function might
                ;;               return a function and not a list) and filter them according to the
                ;;               start and end.
                ;;             - then completion-at-region will show the result of
                ;;               completion-all-completions to the screen.
                ;;     - this infers the completion-at-point will show the completion results to
                ;;       the screen because of the call the completion-at-region.
                ;;
                ;; Here we force the completion-at-region to aggregate the results of the
                ;; completion-all-completions and cause exec-old-completion-at-point to return
                ;; the results instead of showing them. These results are appended to the
                ;; completion-epc result and returned from the ac-completion-at-point function.
                ;; The ac-completion-at-point is registered as completion-in-point-functions,
                ;; so the real completion-in-point will get the results from ac-completion-at-point
                ;; and then will send them to the real completion-at-region.
                ;; the real completion-at-region shows the nice and colored auto-complete menu for
                ;; a final touch.
                (exec-old-completion-at-point
                 (lambda ()
                   (let (
                         (res)
                         )
                     (let (
                           (completion-at-point-functions
                            old-completion-at-point-functions)
                           (result-aggregator
                            (lambda (start end collection &optional predicate)
                              (let (
                                    (cands
                                     (completion-all-completions
                                      (buffer-substring-no-properties start end)
                                      collection predicate
                                      (- end start))
                                     )
                                    )
                                (unless (null cands)
                                  (setcdr (last cands) nil))
                                (setq res (append res cands)))
                              nil))
                           )
                       (letf (
                              ((symbol-function 'completion-in-region)
                               result-aggregator)
                              )
                         (completion-at-point)))
                     res)))

                (ac-completion-at-point
                 (lambda ()
                   (with-current-buffer working-buffer
                     (unless (null mngr-complete-epc)
                       (let (
                             (prefix (funcall prefix-cb))
                             )
                         (when (stringp prefix)
                           (funcall ac-epc-complete-request prefix)
                           (let (
                                 (beg (- (point) (length prefix)))
                                 (end (point))
                                 )
                             (list beg
                                   end
                                   (completion-table-dynamic
                                    (lambda (_)
                                      (funcall ac-epc-matches)))))))))))
                )
           (add-to-list 'ac-sources ac-epc-source)
           (add-hook 'completion-at-point-functions
                     ac-completion-at-point nil t)))))))

(defun epc-completion-add(completion-mode hook prefix-cb)
  (epc-completion-add-company completion-mode hook prefix-cb)
  (epc-completion-add-auto-complete completion-mode hook prefix-cb))


  ; (add-to-list 'ac-omni-completion-sources (cons "\\." '(ac-source-epc-direct)))

; (epc-completion-add (lambda () (substring-no-properties (company-grab-symbol))))

(provide 'completion-epc)
