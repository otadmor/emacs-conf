;; (require 'comint)
;; (require 'url-tramp)
;; (setq hostname-cache (split-string (shell-command-to-string "realm list --name-only") "\n"))

(defun url-generic-parse-url (url)
  "Return an URL-struct of the parts of URL.
The CL-style struct contains the following fields:

TYPE     is the URI scheme (string or nil).
USER     is the user name (string or nil).
PASSWORD is the password (string [deprecated] or nil).
HOST     is the host (a registered name, IP literal in square
         brackets, or IPv4 address in dotted-decimal form).
PORTSPEC is the specified port (a number), or nil.
FILENAME is the path AND the query component of the URI.
TARGET   is the fragment identifier component (used to refer to a
         subordinate resource, e.g. a part of a webpage).
ATTRIBUTES is nil; this slot originally stored the attribute and
         value alists for IMAP URIs, but this feature was removed
         since it conflicts with RFC 3986.
FULLNESS is non-nil if the hierarchical sequence component of
         the URL starts with two slashes, \"//\".

The parser follows RFC 3986, except that it also tries to handle
URIs that are not fully specified (e.g. lacking TYPE), and it
does not check for or perform %-encoding.

Here is an example.  The URL

  foo://bob:pass@example.com:42/a/b/c.dtb?type=animal&name=narwhal#nose

parses to

  TYPE     = \"foo\"
  USER     = \"bob\"
  PASSWORD = \"pass\"
  HOST     = \"example.com\"
  PORTSPEC = 42
  FILENAME = \"/a/b/c.dtb?type=animal&name=narwhal\"
  TARGET   = \"nose\"
  ATTRIBUTES = nil
  FULLNESS = t"
  (if (null url)
      (url-parse-make-urlobj)
    (with-temp-buffer
      ;; Don't let those temp-buffer modifications accidentally
      ;; deactivate the mark of the current-buffer.
      (let ((deactivate-mark nil))
        (set-syntax-table url-parse-syntax-table)
	(erase-buffer)
	(insert url)
	(goto-char (point-min))
        (let ((save-pos (point))
              scheme user pass host port file fragment full
              (inhibit-read-only t))

          ;; 3.1. Scheme
	  ;; This is nil for a URI that is not fully specified.
          (when (looking-at "\\([a-zA-Z][-a-zA-Z0-9+.]*\\):")
	    (goto-char (match-end 0))
            (setq save-pos (point))
	    (setq scheme (downcase (match-string 1))))

          ;; 3.2. Authority
          (when (looking-at "//")
            (setq full t)
            (forward-char 2)
            (setq save-pos (point))
            (skip-chars-forward "^/?#")
            (setq host (buffer-substring save-pos (point)))
	    ;; 3.2.1 User Information
            (if (string-match "^\\([^@]+\\)@" host)
                (setq user (match-string 1 host)
                      host (substring host (match-end 0))))
            (if (and user (string-match "\\`\\([^:]*\\):\\(.*\\)" user))
                (setq pass (match-string 2 user)
                      user (match-string 1 user)))
            (cond
	     ;; IPv6 literal address.
	     ((string-match "^\\(\\[[^]]+\\]\\)\\(?::\\([0-9]*\\)\\)?$" host)
	      (setq port (match-string 2 host)
		    host (match-string 1 host)))
	     ;; Registered name or IPv4 address.
	     ((string-match ":\\([0-9]*\\)$" host)
	      (setq port (match-string 1 host)
		    host (substring host 0 (match-beginning 0)))))
	    (cond ((equal port "")
		   (setq port nil))
		  (port
		   (setq port (string-to-number port))))
            (setq host host))

	  ;; Now point is on the / ? or # which terminates the
	  ;; authority, or at the end of the URI, or (if there is no
	  ;; authority) at the beginning of the absolute path.

          (setq save-pos (point))
          (if (string= "data" scheme)
	      ;; For the "data" URI scheme, all the rest is the FILE.
	      (setq file (buffer-substring save-pos (point-max)))
	    ;; For hysterical raisins, our data structure returns the
	    ;; path and query components together in one slot.
	    ;; 3.3. Path
	    (skip-chars-forward "^?#")
	    ;; 3.4. Query
	    (when (looking-at "\\?")
	      (skip-chars-forward "^#"))
	    (setq file (buffer-substring save-pos (point)))
	    ;; 3.5 Fragment
	    (when (looking-at "#")
	      (let ((opoint (point)))
		(forward-char 1)
                (setq fragment (buffer-substring (point) (point-max)))
		(delete-region opoint (point-max)))))

          (if (and host (string-match "%[0-9][0-9]" host))
              (setq host (url-unhex-string host)))
          (url-parse-make-urlobj scheme user pass host port file
				 fragment nil full))))))

(with-eval-after-load 'tramp
  (setq tramp-allow-unsafe-temporary-files t)
  (defvar tramp-hide-from-history t
    "Dont show tramp commands on bash_history etc")
  (defun tramp-send-command (vec command &optional neveropen nooutput)
    "Send the COMMAND to connection VEC.
Erases temporary buffer before sending the command.  If optional
arg NEVEROPEN is non-nil, never try to open the connection.  This
is meant to be used from `tramp-maybe-open-connection' only.  The
function waits for output unless NOOUTPUT is set."
    (unless neveropen (tramp-maybe-open-connection vec))
    (let ((p (tramp-get-connection-process vec)))
      (when (tramp-get-connection-property p "remote-echo" nil)
        ;; We mark the command string that it can be erased in the output buffer.
        (tramp-set-connection-property p "check-remote-echo" t)
        ;; If we put `tramp-echo-mark' after a trailing newline (which
        ;; is assumed to be unquoted) `tramp-send-string' doesn't see
        ;; that newline and adds `tramp-rsh-end-of-line' right after
        ;; `tramp-echo-mark', so the remote shell sees two consecutive
        ;; trailing line endings and sends two prompts after executing
        ;; the command, which confuses `tramp-wait-for-output'.
        (when (and (not (string-empty-p command))
		   (string-equal (substring command -1) "\n"))
	  (setq command (substring command 0 -1)))
        ;; No need to restore a trailing newline here since `tramp-send-string'
        ;; makes sure that the string ends in `tramp-rsh-end-of-line', anyway.
        (setq command (format "%s%s%s" tramp-echo-mark command tramp-echo-mark)))
      ;; Send the command.
      (tramp-message vec 6 "%s" command)
      (tramp-send-string vec (format "%s%s" (when tramp-hide-from-history " ") command))
      (unless nooutput (tramp-wait-for-output p)))))

(with-eval-after-load 'tramp-adb
  (setq tramp-adb-program (car (split-string (shell-command-to-string "which adb") "\n")))
  (defun tramp-adb-execute-adb-command (vec &rest args)
    "Execute an adb command.
Insert the result into the connection buffer.  Return nil on
error and non-nil on success."
    (when (and (> (length (tramp-file-name-host vec)) 0)
	       ;; The -s switch is only available for ADB device commands.
	       (not (member (car args) '("connect" "disconnect"))))
      (let ((device (tramp-adb-get-device vec)))
        (if (listp device)
            (setq args (append (list "-s") (tramp-adb-get-device vec) args))
          (setq args (append (list "-s" (tramp-adb-get-device vec)) args)))))
    (with-current-buffer (tramp-get-connection-buffer vec)
      ;; Clean up the buffer.  We cannot call `erase-buffer' because
      ;; narrowing might be in effect.
      (let ((inhibit-read-only t)) (delete-region (point-min) (point-max)))
      (zerop (apply #'tramp-call-process vec tramp-adb-program nil t nil args))))


  (defun tramp-adb-get-device (vec)
    "Return full host name from VEC to be used in shell execution.
E.g. a host name \"192.168.1.1#5555\" returns \"192.168.1.1:5555\"
     a host name \"R38273882DE\" returns \"R38273882DE\"."
    (with-tramp-connection-property (tramp-get-process vec) "device"
      (let* ((self-host (system-name))
             (host (tramp-file-name-host vec))
	     (port (tramp-file-name-port-or-default vec))
             (user (tramp-file-name-user vec))
	     (devices (mapcar #'cadr (tramp-adb-parse-device-names nil))))
        (if (not (null user))
            (progn
              (when (not (string= host self-host))
                (let ((forward-buffer-name
                       (format "*tramp-adb-port-forward-%s#%s*" host port)))
                  (unless (get-buffer forward-buffer-name)
                    (with-current-buffer (get-buffer-create forward-buffer-name)
                      (let ((default-directory "/"))
                        (start-process-shell-command forward-buffer-name
                                                     forward-buffer-name
                                                     (format "while [[ 1 ]]; do /bin/bash -c 'i=49152; while (echo \"\" > /dev/tcp/127.0.0.1/$i) >/dev/null 2>&1; do i=$[49152 + ($RANDOM %% 16384)]; done; sleep 0.1; ssh -L $i:localhost:%s %s \"echo ADBPORT=$i; while [[ 1 ]]; do sleep 100; done;\"'; done;" port host))
                        (goto-char (point-max))
                        (while (not (search-backward "ADBPORT=" nil t))
                          (sit-for 0.1)
                          (goto-char (point-max))))))
                  (with-current-buffer (get-buffer forward-buffer-name)
                    (goto-char (point-max))
                    (search-backward "ADBPORT=")
                    (forward-char (length "ADBPORT="))
                    (setq port (buffer-substring-no-properties
                                (point) (line-end-position)))
                    (setq host self-host))))
              (append
               (list (tramp-compat-string-replace
                      tramp-prefix-port-format ":" user))
               (when (and host (not (string= host self-host)))
                 (list "-H" host))
               (when (and port (not (string= port "5037"))) (list "-P" port))))
          (tramp-compat-string-replace
           tramp-prefix-port-format ":"
           (cond ((member host devices) host)
	         ;; This is the case when the host is connected to the default port.
	         ((member (format "%s%s%d" host tramp-prefix-port-format port)
		          devices)
	          (format "%s:%d" host port))
	         ;; An empty host name shall be mapped as well, when there
	         ;; is exactly one entry in `devices'.
	         ((and (zerop (length host)) (= (length devices) 1))
	          (car devices))
	         ;; Try to connect device.
	         ((and tramp-adb-connect-if-not-connected
		       (not (zerop (length host)))
		       (tramp-adb-execute-adb-command
                        vec "connect"
                        (tramp-compat-string-replace
		         tramp-prefix-port-format ":" host)))
	          ;; When new device connected, running other adb command (e.g.
	          ;; adb shell) immediately will fail.  To get around this
	          ;; problem, add sleep 0.1 second here.
	          (sleep-for 0.1)
	          host)
	         (t (tramp-error
		     vec 'file-error "Could not find device %s" host))))))))

  (defun tramp-adb-maybe-open-connection (vec)
    "Maybe open a connection VEC.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
    ;; During completion, don't reopen a new connection.
    (unless (tramp-connectable-p vec)
      (throw 'non-essential 'non-essential))

    (let* ((buf (tramp-get-connection-buffer vec))
           (p (get-buffer-process buf))
           (host (tramp-file-name-host vec))
           (user (tramp-file-name-user vec))
           (device (tramp-adb-get-device vec)))

      (unless (process-live-p p)
        (save-match-data
	  (when (and p (processp p)) (delete-process p))
	  (if (zerop (length device))
	      (tramp-error vec 'file-error "Device %s not connected" host))
	  (with-tramp-progress-reporter vec 3 "Opening adb shell connection"
	    (let* ((coding-system-for-read 'utf-8-dos) ;is this correct?
		   (process-connection-type tramp-process-connection-type)
		   (args (if (> (length host) 0)
                             (if (listp device)
                                 (append (list "-s") device (list "shell"))
			       (list "-s" device "shell"))
			   (list "shell")))
		   (p (let ((default-directory
			      tramp-compat-temporary-file-directory))
		        (apply #'start-process (tramp-get-connection-name vec) buf
			       tramp-adb-program args)))
		   (prompt (md5 (concat (prin1-to-string process-environment)
				        (current-time-string)))))
	      (tramp-message
	       vec 6 "%s" (string-join (process-command p) " "))
	      ;; Wait for initial prompt.  On some devices, it needs an
	      ;; initial RET, in order to get it.
              (sleep-for 0.1)
	      (tramp-send-string vec tramp-rsh-end-of-line)
	      (tramp-adb-wait-for-output p 30)
	      (unless (process-live-p p)
	        (tramp-error vec 'file-error "Terminated!"))

	      ;; Set sentinel and query flag.  Initialize variables.
	      (set-process-sentinel p #'tramp-process-sentinel)
	      (process-put p 'vector vec)
	      (process-put p 'adjust-window-size-function #'ignore)
	      (set-process-query-on-exit-flag p nil)

	      ;; Set connection-local variables.
	      (tramp-set-connection-local-variables vec)

	      ;; Change user if indicated.
	      (when (tramp-get-file-property vec "" "su-command-p" t)
	        (tramp-adb-send-command vec (format "su")))

	      ;; Change prompt.
	      (tramp-set-connection-property
	       p "prompt" (regexp-quote (format "///%s#$" prompt)))
	      (tramp-adb-send-command
	       vec (format "PS1=\"///\"\"%s\"\"#$\"" prompt))

	      ;; Disable line editing.
	      (tramp-adb-send-command
	       vec "set +o vi +o vi-esccomplete +o vi-tabcomplete +o emacs")

	      ;; Dump option settings in the traces.
	      (when (>= tramp-verbose 9)
	        (tramp-adb-send-command vec "set -o"))

	      ;; Check whether the properties have been changed.  If
	      ;; yes, this is a strong indication that we must expire all
	      ;; connection properties.  We start again.
	      (tramp-message vec 5 "Checking system information")
	      (tramp-adb-send-command
	       vec
	       (concat
	        "echo \\\"`getprop ro.product.model` "
	        "`getprop ro.product.version` "
	        "`getprop ro.build.version.release`\\\""))

	      (let ((old-getprop
		     (tramp-get-connection-property vec "getprop" nil))
		    (new-getprop
		     (tramp-set-connection-property
		      vec "getprop"
		      (with-current-buffer (tramp-get-connection-buffer vec)
		        ;; Read the expression.
		        (goto-char (point-min))
		        (read (current-buffer))))))
	        (when (and (stringp old-getprop)
			   (not (string-equal old-getprop new-getprop)))
		  (tramp-message
		   vec 3
		   "Connection reset, because remote host changed from `%s' to `%s'"
		   old-getprop new-getprop)
		  (tramp-cleanup-connection vec t)
		  (tramp-adb-maybe-open-connection vec)))

	      ;; Mark it as connected.
	      (tramp-set-connection-property p "connected" t)))))))

  (defun tramp-adb-get-ls-command (vec)
    "Determine `ls' command and its arguments."
    (with-tramp-connection-property vec "ls"
      (tramp-message vec 5 "Finding a suitable `ls' command")
      (cond
       ;; Support Android derived systems where "ls" command is provided
       ;; by GNU Coreutils.  Force "ls" to print one column and set
       ;; time-style to imitate other "ls" flavors.
       ((tramp-adb-send-command-and-check
         vec (concat "ls --time-style=long-iso "
                     (tramp-get-remote-null-device vec)))
        "ls -1 --time-style=long-iso")
       ;; Can't disable coloring explicitly for toybox ls command.  We
       ;; also must force "ls" to print just one column.
       ((tramp-adb-send-command-and-check vec "toybox") "toybox ls -1")
       ;; On CyanogenMod based system BusyBox is used and "ls" output
       ;; coloring is enabled by default.  So we try to disable it when
       ;; possible.
       ((tramp-adb-send-command-and-check
         vec (concat "ls --color=never -al " (tramp-get-remote-null-device vec)))
        "ls --color=never")
       (t "ls"))))

  (defun tramp-adb-handle-file-name-all-completions (filename directory)
    "Like `file-name-all-completions' for Tramp files."
    (all-completions
     filename
     (with-parsed-tramp-file-name (expand-file-name directory) nil
       (with-tramp-file-property v localname "file-name-all-completions"
         (tramp-adb-send-command
	  v (format "%s -ap %s"
		    (tramp-adb-get-ls-command v)
		    (tramp-shell-quote-argument localname)))
	 (with-current-buffer (tramp-get-buffer v)
	   (delete-dups
	    (append
	     ;; In older Android versions, "." and ".." are not
	     ;; included.  In newer versions (toybox, since Android 6)
	     ;; they are.  We fix this by `delete-dups'.
	     '("./" "../")
	     (delq
	      nil
	      (mapcar
	       (lambda (l) (and (not (string-match-p "^[[:space:]]*$" l)) l))
	       (split-string (buffer-string) "\n")))))))))))

(defun comint-osc-directory-tracker (_ text)
  "Update `default-directory' from OSC 7 escape sequences.

This function is intended to be included as an entry of
`comint-osc-handlers'.  You should moreover arrange for your
shell to print the appropriate escape sequence at each prompt,
say with the following command:

    printf \"\\e]7;file://%s%s\\e\\\\\" \"$HOSTNAME\" \"$PWD\"

This functionality serves as an alternative to `dirtrack-mode'
and `shell-dirtrack-mode'."
  ;; TODO use : url-tramp-convert-url-to-tramp
  (let ((fullpath (let ((url (url-generic-parse-url text))
                        (config-hosts
                         (remove-if
                          (lambda (x) (string= x ""))
                          (split-string
                           (let ((default-directory "/"))
                             (shell-command-to-string
                              "sed -nr \"s/^Host (.*)/\\1/p\" ~/.ssh/config | tr ' ' '\\n'")) "\n"))))
                    (let ((is-filescheme (string= (url-type url) "file"))
                          (is-localhost (or (null (url-host url))
                                            (cl-equalp (url-host url) (system-name))
                                            ;; (cl-member (system-name)
                                            ;;            (mapcar (lambda (a)
                                            ;;                      (concat (url-host url) "." a))
                                            ;;                    hostname-cache)
                                            ;;            :test #'cl-equalp)
                                            ))
                          (is-sameuser (or (null (url-user url))
                                           (string= (url-user url) user-login-name))))
                      (if (and is-filescheme is-localhost is-sameuser)
                          (url-filename url)
                        (when (or is-localhost (cl-member (url-host url)
                                                          config-hosts
                                                          :test #'cl-equalp))
                          (tramp-make-tramp-file-name (make-tramp-file-name
                                                       :method (if is-filescheme
                                                                   (if is-localhost "sudo" "ssh")
                                                                 (url-type url))
                                                       :user (if is-filescheme
                                                                 (or (url-user url) user-login-name)
                                                               (url-user url))
                                                       :host (or (url-host url) (system-name))
                                                       :port (let ((p (url-port-if-non-default url)))
                                                               (if (and (string= (url-type url) "adb")
                                                                        (not (numberp p)))
                                                                   "5037"
                                                                 (if (numberp p)
                                                                     (number-to-string p) p)))
                                                       ;; url-unhex-string
                                                       :localname (url-filename url)))))))))
    (when (and fullpath
               (not (string= default-directory (expand-file-name fullpath))))
      (ignore-errors (cd-absolute (concat fullpath "/"))))))

(defun comint-delete-backward-char(fun &rest args)
  (when (or (region-active-p)
            (not (consp comint-last-prompt))
            (/= (cdr comint-last-prompt) (point)))
    (apply fun args)))

(with-eval-after-load 'comint
  (setq comint-get-old-input (lambda () ""))
  (ansi-color-for-comint-mode-on)

  (setq comint-password-prompt-regexp
        (concat comint-password-prompt-regexp
                "\\|^Password for \\\\'.*\\\\':\\s *\\'"))
  (setq comint-password-prompt-regexp
        (concat comint-password-prompt-regexp
                "\\|^Password for '.*':\s+"))
  (setq comint-password-prompt-regexp
        (concat comint-password-prompt-regexp
                "\\|^.*\\\\' password:\\s *\\'"))

  (advice-add 'delete-backward-char :around #'comint-delete-backward-char)

  (add-hook 'comint-mode-hook (lambda ()
                                (shell-dirtrack-mode 0)
                                (dirtrack-mode 0)
                                ;; (setenv "PROMPT_COMMAND" (format "history -a"))
                                ))
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output))
(provide 'comint-ext)
