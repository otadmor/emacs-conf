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
                        (when (or (not is-filescheme) is-localhost (cl-member
                                                                    (url-host url)
                                                                    config-hosts
                                                                    :test #'cl-equalp))
                          (tramp-make-tramp-file-name (make-tramp-file-name
                                                       :method (if is-filescheme
                                                                   (if is-localhost "sudo" "ssh")
                                                                 (url-type url))
                                                       :user (if is-filescheme
                                                                 (or (url-user url) user-login-name)
                                                               (url-user url))
                                                       :host (if is-filescheme
                                                                 (or (url-host url) "localhost")
                                                               (url-host url))
                                                       :port (url-port-if-non-default url)
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
