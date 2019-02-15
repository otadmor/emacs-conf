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

(defun disconnect-completion-server()
  (when mngr-complete-epc
    (setq-local mngr-complete-epc nil)
    (kill-local-variable 'mngr-complete-epc)
    )
  )

(defun fire-exit-hook(orig-fun &rest args)
  (condition-case err
      (apply orig-fun args)
    (error (epc:log "Error on exit-hooks : %S / " err mngr)))
  (disconnect-completion-server)
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
  (message "Found completion server at %S" server-port)
  (let (
        (mngr (epc:connect-remote-server
               "localhost"
               server-port))
        )
    (setq-local mngr-complete-epc mngr)
    (epc:manager-add-exit-hook mngr 'disconnect-completion-server)))



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

(defun epc-complete-deferred (to-complete)
  ;(message "Try to complete %S" to-complete)
  (when mngr-complete-epc
    (condition-case nil
        (epc:call-deferred mngr-complete-epc 'complete to-complete)
        (error (progn (message "error in completion server") (disconnect-completion-server) nil)))))


(defun epc-complete (to-complete)
  ;(message "Try to complete %S" to-complete)
  (when mngr-complete-epc
    (condition-case nil
        (let (
              (completions (epc:call-sync mngr-complete-epc 'complete to-complete))
              )
          ;(message "Return : %S" completions)
          completions
          )
      (error (progn (message "error in completion server") (disconnect-completion-server) nil)))
    ))


(defun epc-symbol (candidate)
  ;(message "Try to complete %S" to-complete)
  (when mngr-complete-epc
    (condition-case nil
        (let (
              (symbol (epc:call-sync mngr-complete-epc 'symbol candidate))
              )
          ;(message "Return : %S" completions)
          symbol
          )
      (error (progn (message "error in completion server") (disconnect-completion-server) nil)))
    ))

(defun epc-meta (candidate)
  ;(message "Try to complete %S" to-complete)
  (when mngr-complete-epc
    (condition-case nil
        (let (
              (meta (epc:call-sync mngr-complete-epc 'meta candidate))
              )
          ;(message "Return : %S" completions)
          meta
          )
      (error (progn (message "error in completion server") (disconnect-completion-server) nil)))
    ))

(defun epc-doc (candidate)
  ;(message "Try to complete %S" to-complete)
  (when mngr-complete-epc
    (condition-case nil
        (let (
              (doc (epc:call-sync mngr-complete-epc 'doc candidate))
              )
          ;(message "Return : %S" completions)
          doc
          )
      (error (progn (message "error in completion server") (disconnect-completion-server) nil)))
    ))

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

(defun completion-epc-candidates(prefix)
  (epc-complete prefix))

(defun completion-epc-complete-deferred(prefix)
  (cons :async
        (lambda (callback)
          (deferred:nextc
            (epc-complete-deferred prefix)
            (lambda (reply)
              (let ((candidates (mapcar 'completion-epc-collect-candidates reply)))
                (funcall callback candidates)))))))

(defun epc-completion-at-point ()
  (when (comint--match-partial-filename)
    (let (
          (start (nth 0 (match-data)))
          (end (nth 1 (match-data)))
          )
      (when (and start end (< start end))
        (let (
              (to-complete (buffer-substring-no-properties start end))
              )
          (let (
                ;;; TODO - for some reason epc-complete runs twice,
                ;; which is very expensive.
                (completions (epc-complete to-complete))
                )
            (when completions
              (list start end completions ())
              )))))))

; hook both, just in case it runs without or with shell
(add-hook 'shell-dynamic-complete-functions 'epc-completion-at-point nil nil)
(add-hook 'comint-dynamic-complete-functions 'epc-completion-at-point nil nil)

(require 'cl-lib)
(require 'company)

(defun epc-completion-add(prefix-cb)
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
    (eval-after-load 'company '(add-to-list 'company-backends completion-func))))

(epc-completion-add
 (lambda () (and (eq major-mode 'fundamental-mode) (company-grab-symbol))))

(provide 'completion-epc)
