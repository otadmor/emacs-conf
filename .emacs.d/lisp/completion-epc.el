;;;  -*- lexical-binding: t -*-
;; (require 'epc)
;; (require 'comint)
;; (require 'epcs)

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

(defun epc:with-manager-for-connection (connection func)
  (cl-loop
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
    ;; (make-local-variable 'process-environment)
    ;; (let ((process-environment (cons "LANG=" process-environment)))
    (setenv "EPC_COMPLETION_SERVER_PORT" (format "%d" port))))

;; usage : (epc-gdb-command-mngr mngr-complete-epc "context regs" (lambda (x) (message "REGS=%S" x)))
(defun epc-gdb-command-mngr (mngr command callback &optional errback)
  (lexical-let ((errback errback))
    (deferred:$
      (epc:call-deferred mngr 'gdbcommand command)
      (deferred:nextc it callback)
      (deferred:error it
        (lambda (err) (if (null errback)
                          (message "error %S" err)
                        (funcall errback err)))))))

(defun epc-gdb-command (command &optional callback errback)
  (interactive "sGDB Command: ")
  (when (null callback)
    (setq callback (lambda (x) (message "%S" x))))
  (epc-gdb-command-mngr mngr-complete-epc command callback errback))

(defun epc-gdb-get-source (lib-offset callback)
  (deferred:$
    (epc:call-deferred mngr 'idagetsource lib-offset)
    (deferred:nextc it callback)
    (deferred:error it
      (lambda (err) (message "error getting source %S" err)))))

(defun goto-source-line (filename lineno)
  (condition-case nil
      (let (
            (my-buffer (find-file-noselect filename))
            )
        ;; (message "buffer is %S" my-buffer)
        (let (
              (my-window (display-buffer my-buffer))
              )
          ;; (message "window is %S" my-window)
          (with-selected-window my-window
            (goto-line lineno)
            ;; (message "went to line")
            (winstack-push nil t))))
    (error nil)))

(defface coverage-high-covered-face
  '((((class color) (background light))
     :background "#aaccaa")
    (((class color) (background dark))
     :background "#113311"))
  "The face used for different coverage alongside the original coverage"
  :group 'coverage)

(setq --gdb-epc--gdb-mi-loaded nil)
(with-eval-after-load 'gdb-mi
  (setq --gdb-epc--gdb-mi-loaded t))
(require 'gdb-mi)
(defun gdb-apply-faces ()
  (beginning-of-buffer)
  (while (/= (point) (point-max))
    (when (get-text-property (point) 'executed)
      (let ((execute-overlay (make-overlay (point) (+ (line-end-position) 1))))
        (overlay-put execute-overlay 'face 'coverage-covered-face)))
    (beginning-of-line 2))
  (when --gdb-epc--gdb-mi-loaded
    (beginning-of-buffer)
    (while (/= (point) (point-max))
      (when (get-text-property (point) 'break)
        (gdb-put-breakpoint-icon t 0))
      (beginning-of-line 2))))

(defun gdb-parse-asm-lines (lib asm-lines)
  (with-temp-buffer (insert
                     (mapconcat
                      (lambda (x) (let ((line (cadr x)))
                                    (put-text-property 0 1 'executed (car x)    line)
                                    (put-text-property 0 1 'addr     (caddr x)  line)
                                    (put-text-property 0 1 'lib      lib        line)
                                    (put-text-property 0 1 'break    (cadddr x) line)
                                    line))
                      asm-lines "\n"))
                    (insert "\n")
                    (asm-mode)
                    (font-lock-ensure)
                    (buffer-substring (point-min) (point-max))))

(defun gdb-parse-c-lines (c-lines)
  (with-temp-buffer (insert
                     (mapconcat
                      (lambda (x) (let ((line (concat "        " (cadr x))))
                                    (put-text-property 0 1 'executed (car x)    line)
                                    line))
                      c-lines "\n"))
                    (insert "\n")
                    (c-mode)
                    (font-lock-ensure)
                    (buffer-substring (point-min) (point-max))))

(defun elisp--find-overlays-specifying (prop)
  (let ((overlays (overlays-at (point)))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun gdb-epc-code-show-or-hide-assembly()
  (interactive)
  (let ((code-overlays (elisp--find-overlays-specifying 'asm-overlay)))
    (dolist (code-overlay code-overlays)
      (let ((asm-overlay (overlay-get code-overlay 'asm-overlay)))
        (overlay-put asm-overlay 'invisible (not (overlay-get asm-overlay 'invisible)))))))

(defun gdb-epc-asm-hide-assembly()
  (interactive)
  (let ((asm-overlays (elisp--find-overlays-specifying 'i-am-asm)))
    (dolist (asm-overlay asm-overlays)
      (when (overlay-get asm-overlay 'i-am-asm)
        (goto-char (- (overlay-start asm-overlay) 1))
        (overlay-put asm-overlay 'invisible t)))))

(defun gdb-epc-call-for-addr (addr callback)
  (save-excursion
    (beginning-of-buffer)
    (while (/= (point) (point-max))
      (let ((current-address (get-text-property (point) 'addr)))
        (when (and (not (null current-address)) (= current-address addr))
          (funcall callback (point))))
      (beginning-of-line 2))))

(defun gdb-epc-set-breakpoint (lib addr)
  (lexical-let (
                (addr addr)
                (source-buffer (current-buffer))
                )
    (gdb-epc-call-for-addr addr (lambda (pos) (let ((inhibit-read-only t))
                                              (put-text-property pos (+ pos 1) 'in-op t))))
    (epc-gdb-command-mngr (with-current-buffer --comint-buffer mngr-complete-epc) (format "bo %s:%X" lib addr)
                          (lambda (res)
                            (let ((breakpoint-id (string-to-number (cadr (split-string res)))))
                              (with-current-buffer source-buffer
                                (gdb-epc-call-for-addr addr (lambda (pos)
                                                              (gdb-put-breakpoint-icon t 0)
                                                              (let ((inhibit-read-only t))
                                                                (put-text-property pos (+ pos 1) 'break
                                                                                   (cons breakpoint-id (get-text-property pos 'break)))
                                                                (put-text-property pos (+ pos 1) 'in-op nil)))))))
                          (lambda (res)
                              (with-current-buffer source-buffer
                                (gdb-epc-call-for-addr addr (lambda (pos)
                                                              (let ((inhibit-read-only t))
                                                                (put-text-property pos (+ pos 1) 'in-op nil)))))))))

(defun gdb-epc-unset-breakpoint (breakpoint-id)
  (lexical-let (
                (addr (get-text-property (point) 'addr))
                (source-buffer (current-buffer))
                (breakpoint-id breakpoint-id)
                )
    (gdb-epc-call-for-addr addr (lambda (pos) (let ((inhibit-read-only t))
                                             (put-text-property pos (+ pos 1) 'in-op t))))
    (epc-gdb-command-mngr (with-current-buffer --comint-buffer mngr-complete-epc) (format "delete %d" breakpoint-id)
                          (lambda (res)
                            (with-current-buffer source-buffer
                              (gdb-epc-call-for-addr addr (lambda (pos)
                                              (gdb-remove-breakpoint-icons pos pos)
                                              (let ((inhibit-read-only t))
                                                (put-text-property pos (+ pos 1) 'break
                                                                   (delq breakpoint-id (get-text-property pos 'break)))
                                                (put-text-property pos (+ pos 1) 'in-op nil))))))
                          (lambda (res)
                            (with-current-buffer source-buffer
                              (gdb-epc-call-for-addr addr (lambda (pos)
                                              (let ((inhibit-read-only t))
                                                (put-text-property pos (+ pos 1) 'in-op nil)))))))))

(defun gdb-epc-asm-toggle-breakpoint()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (get-text-property (point) 'in-op)
      (let (
            (breakpoint-nums (get-text-property (point) 'break))
            (addr (get-text-property (point) 'addr))
            (lib (get-text-property (point) 'lib))
            )
        (if (/= (length breakpoint-nums) 0)
            (if (/= (length breakpoint-nums) 1)
                (message "Too many breakpoints in %S to decide" addr)
              (gdb-epc-unset-breakpoint (car breakpoint-nums)))
          (gdb-epc-set-breakpoint lib addr))))))

(defvar code-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(tab)] 'gdb-epc-code-show-or-hide-assembly)
    map)
  "Code keymap to show or hide assembly")

(defvar asm-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(tab)] 'gdb-epc-asm-hide-assembly)
    (define-key map [f2] 'gdb-epc-asm-toggle-breakpoint)
    map)
  "Code keymap to hide assembly")

(setq --epc-gdb--orig-command-error-function command-error-function)
(defun ignore-readonly-error-function (data context caller)
  (when (not (eq (car data) 'buffer-read-only))
    (funcall --epc-gdb--orig-command-error-function data context caller)))

(defun gdb-request-parse-source-asm-lines (working-buffer lib-offset)
  (lexical-let ((working-buffer working-buffer) (lib (car lib-offset)))
    (epc-gdb-get-source lib-offset
                        (lambda (source)
                          (unless (null source)
                            (setq global-source source)
                            (with-current-buffer working-buffer
                              (with-current-buffer (get-buffer-create (concat (buffer-name) "-debugger-source"))
                                (setq-local --comint-buffer working-buffer)
                                (setq buffer-read-only t)
                                (let ((inhibit-read-only t))
                                  (setq-local command-error-function #'ignore-readonly-error-function)
                                  (erase-buffer)
                                  (when --gdb-epc--gdb-mi-loaded
                                    (gdb-remove-breakpoint-icons (point-min) (point-max)))
                                  (let ((c-source (gdb-parse-c-lines source)))
                                    (insert c-source))
                                  (beginning-of-buffer)
                                  (let ((first-shown nil))
                                    (dolist (x source)
                                      (let ((asm-lines (gdb-parse-asm-lines (file-name-nondirectory lib) (caddr x))))
                                        (if (string= asm-lines "\n")
                                            (end-of-line 2)
                                          (let ((code-overlay (make-overlay (line-beginning-position) (+ (line-end-position) 1) nil t)))
                                            (beginning-of-line 2)
                                            (let (
                                                  (asm-begin (point))
                                                  (line-executed (get-text-property (overlay-start code-overlay) 'executed))
                                                  )
                                              (insert asm-lines)
                                              (let ((asm-line-count (count-lines asm-begin (- (point) 1))))
                                                (goto-char asm-begin)
                                                (dotimes (i asm-line-count)
                                                  (let* (
                                                         (asm-addr (get-text-property (point) 'addr))
                                                         (addr-str (format "%s:%08x" (file-name-nondirectory lib) asm-addr))
                                                         (addr-str-len (length addr-str))
                                                         )
                                                    (let* (
                                                           (line-start (point))
                                                           (asm-executed (get-text-property line-start 'executed))
                                                           )
                                                      (end-of-line)
                                                      (let* (
                                                             (line-end (point))
                                                             (line-len (- line-end line-start -1))
                                                             (line-len-format (format "%%%ds" (- 120 line-len addr-str-len)))
                                                             )
                                                        (insert (propertize (format line-len-format addr-str)
                                                                            'executed asm-executed
                                                                            'addr asm-addr)))))
                                                  (beginning-of-line 2)))
                                              (let ((asm-end (point)))
                                                (let ((asm-overlay (make-overlay asm-begin asm-end nil t)))
                                                  (overlay-put code-overlay 'asm-overlay asm-overlay)
                                                  (overlay-put code-overlay 'keymap code-overlay-map)
                                                  (overlay-put asm-overlay 'i-am-asm t)
                                                  (overlay-put asm-overlay 'keymap asm-overlay-map)
                                                  (if line-executed
                                                      (overlay-put asm-overlay 'face 'coverage-high-covered-face)
                                                    (overlay-put asm-overlay 'face 'coverage-uncovered-face))
                                                  (let ((code-executed (get-text-property (overlay-start code-overlay) 'executed)))
                                                    (when (or (not code-executed) first-shown)
                                                      (overlay-put asm-overlay 'invisible t))
                                                    (when code-executed
                                                      (setq first-shown t)))))))))))
                                  (gdb-apply-faces)))))))))

(defun debugger-stop-event(working-buffer filename lineno context code lib-offset)
  ;; (message "STOPPED AT %S:%S" filename lineno)
  ;; (message "%S" lib-offset)
  (with-current-buffer working-buffer
    (with-current-buffer (get-buffer-create (concat (buffer-name) "-debugger-context"))
      (erase-buffer)
      (insert context)
      (ansi-color-apply-on-region (point-min) (point-max)))
    (with-current-buffer (get-buffer-create (concat (buffer-name) "-debugger-code"))
      (erase-buffer)
      (insert code)
      (ansi-color-apply-on-region (point-min) (point-max)))
    (if (null filename)
        (gdb-request-parse-source-asm-lines working-buffer lib-offset)
      (goto-source-line filename lineno))))

(defun register-post-connection-made-callbacks (mngr working-buffer)
  (lexical-let ((working-buffer working-buffer))
    (epc:define-method mngr 'debugger-stop-event (lambda (filename lineno context code lib-offset) (debugger-stop-event working-buffer filename lineno context code lib-offset)) "args" "notify emacs for breakpoint.")))

(defun epc:incoming-connection-made-mngr (mngr working-buffer)
  (with-current-buffer working-buffer
    (message "EPC client connected in buffer %S" (current-buffer))
    (setq-local mngr-complete-epc mngr)
    (register-post-connection-made-callbacks mngr working-buffer)))

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


(with-eval-after-load 'comint
  (add-hook 'comint-output-filter-functions 'completion--comint-output-filter nil nil))

(with-eval-after-load 'epc
  (defalias 'epc:net-read-or-lose 'epc:net-read-or-lose-fix)
  (advice-add 'epc:stop-epc :around #'completion-epc-clear-completion-epc-hook))

;; (require 'cl-lib)
;; (require 'company)
;; (require 'auto-complete)

(defun epc-completion-add-company (prefix-cb)
  (with-eval-after-load 'company
    (let (
          (completion-func
           (lambda (command &optional arg &rest ignored)
             (interactive (list 'interactive))
             (cl-case command
               (interactive (company-begin-backend 'completion-func))
               (prefix (unless (null mngr-complete-epc)
                         (funcall prefix-cb)))
               (candidates (cons :async
                                 (lambda (callback)
                                   (deferred:$
                                     (epc-complete-deferred arg)
                                     (deferred:nextc it
                                       (lambda (reply)
                                         (let ((candidates
                                                (mapcar 'completion-epc-collect-candidates reply)))
                                           (funcall callback candidates))))))))
               (meta (get-text-property 0 :symbol arg))
               (doc-buffer (company-doc-buffer (get-text-property 0 :doc arg)))
               (annotation (get-text-property 0 :description arg))
               (location nil)
               (no-cache t)
               (sorted t))
             ))
          )
      (make-local-variable 'company-backends)
      (add-to-list 'company-backends completion-func))))


(defun epc-completion-add-auto-complete (completion-mode working-buffer prefix-cb epc-complete-request epc-matches)
  (with-eval-after-load 'auto-complete
    (let* (
           (epc-ac-completion-prefix
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
             (list 'init epc-complete-request)
             (list 'candidates epc-matches)
             (list 'prefix epc-ac-completion-prefix)))
           )
      (unless (memq completion-mode ac-modes)
        (add-to-list 'ac-modes completion-mode))
      (add-to-list 'ac-sources ac-epc-source))))

(defun epc-completion-add-completion-at-point (working-buffer prefix-cb epc-complete-request epc-matches)
  (let (
        (epc-completion-at-point
         (lambda ()
           (with-current-buffer working-buffer
             (unless (null mngr-complete-epc)
               (let (
                     (prefix (funcall prefix-cb))
                     )
                 (when (stringp prefix)
                   (funcall epc-complete-request prefix)
                   (let (
                         (beg (- (point) (length prefix)))
                         (end (point))
                         )
                     (list beg
                           end
                           (completion-table-dynamic
                            (lambda (_)
                              (funcall epc-matches)))))))))))
        )
    (add-hook 'completion-at-point-functions epc-completion-at-point nil t)))


(defun epc-completion-build-completion-results (res)
  (cl-remove-if
   'null
   (mapcar
    (lambda (x)
      (unless (null x)
        (if (stringp x)
            (popup-make-item x)
          (condition-case nil
              (let* (
                     (word (plist-get x :word))
                     (doc-or-empty (plist-get x :doc))
                     (doc (unless (equal doc-or-empty "")
                            doc-or-empty))
                     (description (plist-get x :description))
                     (symbol (plist-get x :symbol))
                     (pos (plist-get x :pos))
                     )
                (if (functionp 'popup-make-item)
                    (let (
                          (i (popup-make-item
                              word
                              :symbol symbol
                              :document doc
                              :summary description))
                          )
                      (put-text-property 0 1 :pos pos i)
                      i)
                  (let (
                        (i (propertize
                            word
                            'symbol symbol
                            'document doc
                            'summary description))
                        )
                    (put-text-property 0 1 :pos pos i)
                    i)))
            (error nil)))))
    res)))

(defun epc-completion-add (completion-mode hook prefix-cb)
  (add-hook
   hook
   (lambda ()
     ;; epcs must be loaded before comint does
     ;; its comint-mode-hook. once the process has
     ;; started we will not be able to change its
     ;; environment variables and set the correct
     ;; completion server port.
     (require 'epcs)
     (require 'epc)
     (epc:start-server-and-set-env)
     (let (
           (epc-completion--complete-reply nil)
           (epc-completion--deferred nil)
           (epc-completion--received-response nil)
           (epc-completion--working-buffer (current-buffer))
           )
       (let* (
              (epc-prefix
               (lambda ()
                 (with-current-buffer epc-completion--working-buffer
                   (unless (null mngr-complete-epc)
                     (if (string= (epc:manager-status-connection-process mngr-complete-epc) "closed")
                         (setq mngr-complete-epc nil)
                       (funcall prefix-cb))))))

              (epc-matches
               (lambda ()
                 (with-current-buffer epc-completion--working-buffer
                   (unless epc-completion--received-response
                     (deferred:sync! epc-completion--deferred))
                   (setq epc-completion--deferred nil)
                   (unless (null epc-completion--complete-reply)
                     ;; should we creat ea timer and wait for responses
                     (epc-completion-build-completion-results
                      epc-completion--complete-reply)))))

              (epc-complete-request
               (lambda (&optional prefix)
                 (with-current-buffer epc-completion--working-buffer
                   (let (
                         (prefix (or prefix (funcall prefix-cb)))
                         )
                     (unless (null epc-completion--deferred)
                       (deferred:cancel epc-completion--deferred))
                     (setq epc-completion--complete-reply nil)
                     (setq epc-completion--received-response nil)
                     (setq epc-completion--deferred
                           (deferred:$
                             (deferred:next
                               (lambda ()
                                 (with-current-buffer epc-completion--working-buffer
                                   (epc-complete-deferred prefix))))
                             (deferred:nextc it
                               (lambda (reply)
                                 (with-current-buffer epc-completion--working-buffer
                                   (setq epc-completion--complete-reply
                                         reply)
                                   (setq epc-completion--received-response t))))
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
              )
         (epc-completion-add-company epc-prefix)
         (epc-completion-add-auto-complete
          completion-mode epc-completion--working-buffer
          epc-prefix epc-complete-request epc-matches)
         (epc-completion-add-completion-at-point
          epc-completion--working-buffer
          epc-prefix epc-complete-request epc-matches))))))


(provide 'completion-epc)
