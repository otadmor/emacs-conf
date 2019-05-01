;;;  -*- lexical-binding: t -*-
(require 'epc)
(require 'shell)
(require 'comint)

; (defvar complete-server-regexp
;   (concat "^" (regexp-quote "___EPCCompletionServer_PORT=") "\\([[:xdigit:]]+\\)\n$")
;   "How the port of `completion' the server is shown to the screen ")

(defvar complete-server-regexp
  (concat (regexp-quote "___EPCCompletionServer_PORT=") "\\([[:xdigit:]]+\\)\n")
  "How the port of `completion' the server is shown to the screen ")


(setq mngr-complete-epc nil)
(make-local-variable 'mngr-complete-epc)

(defun disconnect-completion-server (mngr)
  (when (not (null mngr))
    (when (eq mngr mngr-complete-epc)
      (setq-local mngr-complete-epc nil)
      (kill-local-variable 'mngr-complete-epc))))

(defun fire-exit-hook(orig-fun &rest args)
  (condition-case err
      (apply orig-fun args)
    (error (epc:log "Error on exit-hooks : %S / " err mngr)))
  (disconnect-completion-server (car args))
  )
(advice-add 'epc:manager-fire-exit-hook :around #'fire-exit-hook)


(defun process-sentinel-hook(orig-fun &rest args)
  (apply orig-fun args)
  (let (
        (connection (car args))
        )
    (let (
          (conn-name (epc:connection-name connection))
          )
      (dolist (mngr epc:live-connections)
        (let (
              (cur-conn-name (epc:connection-name (epc:manager-connection mngr)))
              )
          (if (string= cur-conn-name conn-name)
              (condition-case err
                  (epc:manager-fire-exit-hook mngr)
                (error (epc:log "Error on exit-hooks : %S / " err mngr)))))))))
(advice-add 'epc:process-sentinel :around #'process-sentinel-hook)



(defun epc:connect-remote-server (server-address server-port)
  (let (
        (mngr (make-epc:manager
              :connection (epc:connect server-address server-port)
              ))
        )
    (epc:init-epc-layer mngr)
    mngr))


(defun connect-completion-server (server-port)
  (message "Found EPC server at %S" server-port)
  (let (
        (mngr (epc:connect-remote-server
               "localhost"
               server-port))
        )
    (setq-local mngr-complete-epc mngr)
    (epc:manager-add-exit-hook mngr (lambda () (disconnect-completion-server mngr)))))


(defvar epc-server-process nil
  "Saves the current session server")
(make-local-variable 'epc-server-process)
(defun epc:listen (&optional host port)
  "[internal] Connect the server, initialize the process and
return epc:connection object."
  (epc:log ">> Listening:%s" port)
  (lexical-let* ((connection-id (epc:uid))
                 (connection-name (format "epc server %s" connection-id)))
    (make-network-process
           :name connection-name
           :sentinel #'epc-server-sentinel
           :filter #'epc-server-process-filter
           :server t
           :reuseaddr t
           :family 'ipv4
           :host (or host 'local)
           :service (or port t))))

(defun epc:start-server-and-set-env ()
  (setq epc-server-process (epc:listen))
  (let (
        (port (cadr (process-contact epc-server-process)))
        )
    (message "EPC Server port %S" port)
    (make-local-variable 'process-environment)
    (setenv "EPC_COMPLETION_SERVER_PORT" (format "%d" port))))

(cl-defun epc-server-process-filter (proc string)
  (epc:log "Received data %S" string))

(defun epc-server-sentinel (proc msg)
  "The process sentinel for EPC server connections."
  ;; If this is a new client process, set the query-on-exit flag to nil
  ;; for this process (it isn't inherited from the server process).
  (when (and (eq (process-status proc) 'open)
	     (process-query-on-exit-flag proc))
    (set-process-query-on-exit-flag proc nil)
    (epc-server--new-connection proc))
  ;; Delete the associated connection file, if applicable.
  (and (process-contact proc :server)
       (eq (process-status proc) 'closed)
       (ignore-errors
	 (delete-file (process-get proc :server-file))))
  (epc:log (format "Status changed to %s: %s" (process-status proc) msg))
  ; (server-delete-client proc)
  )

(defun epc:incoming-connection-made (connection)
  (let (
        (mngr (make-epc:manager :connection connection))
        )
    (message "EPC client connected")
    (epc:init-epc-layer mngr)
    (setq mngr-complete-epc mngr)
    (epc:manager-add-exit-hook mngr (lambda () (disconnect-completion-server mngr)))))



(defun epc-server--new-connection (proc)
  "[internal] Connect the server, initialize the process and
return epc:connection object."
  (epc:log ">> New connection")
  (lexical-let* ((connection-id (epc:uid))
                 (connection-name (format "epc con %s" connection-id))
                 (connection-buf (epc:make-procbuf (format "*%s*" connection-name)))
                 (connection-process proc)
                 (channel (cc:signal-channel connection-name))
                 (connection (make-epc:connection
                              :name connection-name
                              :process connection-process
                              :buffer connection-buf
                              :channel channel)))
    (set-process-buffer connection-process connection-buf)
    (epc:log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (epc:process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (epc:process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    (epc:incoming-connection-made connection)))



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
      (error (progn (message "error in completion server") (disconnect-completion-server mngr) nil)))))

(defun epc-complete-deferred (to-complete)
  ;;(message "Try to complete %S" to-complete)
  (epc-complete-deferred-mngr mngr-complete-epc to-complete))

; (defun epc-complete (to-complete)
;   ;(message "Try to complete %S" to-complete)
;   (when mngr-complete-epc
;     (condition-case nil
;         (let (
;               (completions (epc:call-sync mngr-complete-epc 'complete to-complete))
;               )
;           ;(message "Return : %S" completions)
;           completions
;           )
;       (error (progn (message "error in completion server") (disconnect-completion-server mngr) nil)))
;     ))



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
          (let (
                (epc-deferred (epc-complete-deferred prefix))
                )
            (if (null epc-deferred)
                (funcall callback '())
              (deferred:nextc
                epc-deferred
                (lambda (reply)
                  (let ((candidates (mapcar 'completion-epc-collect-candidates reply)))
                    (funcall callback candidates)))))))))

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
    (let* (
           (ac-epc-complete-reply nil)

           (ac-epc-matches
            (lambda ()
              (unless (null ac-epc-complete-reply)
                ; should we creat ea timer and wait for responses
                (mapcar
                 (lambda (x)
                   (destructuring-bind (&key word doc description symbol)
                       x
                     (popup-make-item word
                                      :symbol symbol
                                      :document (unless (equal doc "") doc)
                                      :summary description)))
                 ac-epc-complete-reply))))

           (ac-epc-complete-request
            (lambda ()
              (let (
                    (prefix (funcall prefix-cb))
                    )

                (let (
                      (epc-deferred (epc-complete-deferred prefix))
                      )
                  (unless (null epc-deferred)
                    (deferred:nextc epc-deferred
                      (lambda (reply)
                        (setq-local ac-epc-complete-reply reply))))))))

           (ac-completion-prefix
            (lambda () (let (
                             (prefix (funcall prefix-cb))
                             )
                         (when (stringp prefix)
                           (- (point) (length prefix))))))
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
                  (deferred:nextc (funcall ac-epc-complete-request prefix)
                    (lambda ()
                      (let ((ac-expand-on-auto-complete expand))
                        (ac-start :triggered 'command))))))))

           (ac-completion-at-point
            (lambda ()
              ac-epc-complete-reply))
           )
      (add-hook hook
                (lambda ()
                  (make-local-variable 'ac-epc-complete-reply)
                  (setq-local ac-epc-complete-reply nil)
                  (add-to-list 'ac-sources ac-epc-source)
                  (add-hook 'completion-at-point-functions
                            ac-completion-at-point)))
      ac-completion-func)))

(defun epc-completion-add(completion-mode hook prefix-cb)
  (epc-completion-add-company completion-mode hook prefix-cb)
  (epc-completion-add-auto-complete completion-mode hook prefix-cb))


  ; (add-to-list 'ac-omni-completion-sources (cons "\\." '(ac-source-epc-direct)))

; (epc-completion-add (lambda () (substring-no-properties (company-grab-symbol))))

(provide 'completion-epc)
