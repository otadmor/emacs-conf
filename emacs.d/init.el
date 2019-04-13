
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'package)

(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")  ;; definitions from...
(setq save-abbrevs 'silent)        ;; save abbrevs when files are saved

;(require 'server)
;(setq server-use-tcp t
;      server-host "192.168.1.116"
;      server-socket-dir "~/.emacs.d/server")
;(unless (server-running-p)
;    (server-start))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(default ((t (:foundry "adobe"  :slant normal :weight normal :height 120 :width normal))))
 )

;; ; '(default ((t (:foreground "white" :background "black" ))) t)
;; ;(set-face-attribute 'default nil :font "monospace 10" )
;; ;:family "courier"
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:foundry "adobe" :foreground "white" :background "black" :slant normal :weight normal :height 120 :width normal))))
;;  '(diff-added ((t (:background "#003000"))))
;;  '(diff-hunk-header ((t (:background "blue"))))
;;  '(diff-removed ((t (:background "#300000"))))
;;  '(font-lock-comment-face ((t (:foreground "green"))))
;;  '(font-lock-doc-face ((t (:foreground "green2"))))
;;  '(font-lock-function-name-face ((t (:foreground "gold"))))
;;  '(font-lock-keyword-face ((t (:foreground "red"))))
;;  '(font-lock-preprocessor-face ((t (:foreground "yellow"))))
;;  '(font-lock-string-face ((t (:foreground "cyan"))))
;;  '(font-lock-type-face ((t (:foreground "green3"))))
;;  '(font-lock-variable-name-face ((t (:foreground "aquamarine"))))
;;  '(font-lock-warning-face ((t (:foreground "#Ea0" :bold t))))
;;  '(isearch ((t (:background "cornflowerblue"))))
;;  '(region ((t (:background "#444444"))))
;;  '(show-paren-match ((t (:background "#444464"))))
;;  '(show-paren-mismatch ((t (:background "#600000")))))

;; (set-cursor-color "white")
;; (set-mouse-color "white")

(tool-bar-mode -1)
(setq inhibit-splash-screen t)

;; Prevent the annoying beep on errors
(setq visible-bell t)

(blink-cursor-mode -1)
(delete-selection-mode 1)

(global-eldoc-mode -1)

;; Speedbar settings
(setq speedbar-default-position 'left)
(setq speedbar-frame-parameters '((minibuffer . nil)
                                  (width . 20)
                                  (border-width . 0)
                                  (menu-bar-lines . 0)
                                  (tool-bar-lines . 0)
                                  (unsplittable . t)
                                  (left-fringe . 0)
                                  (left . 0)
                                  ))

; Do not create back-up files
(setq make-backup-files nil)

; Clean-up trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; I hate tabs!
(setq-default indent-tabs-mode nil)

(setq show-paren-style 'expression)
(setq show-paren-delay 0)
(show-paren-mode)

(setq select-enable-clipboard t)

(setq c-basic-offset 4)

(column-number-mode t)
(line-number-mode t)

(set-default 'fill-column 80)

(global-hi-lock-mode 1)

(ivy-mode t)
;(icomplete-mode t)
(setq ivy-use-virtual-buffers t
      ivy-count-format "%d/%d ")
(fset 'yes-or-no-p 'y-or-n-p)

(setq global-mark-ring-max 1000)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 1) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; display stuff
(defun init-scratch()
  (save-excursion
    (switch-to-buffer "*scratch*")
    (setf (buffer-string) "")
    (insert ";;")
    (newline)
    (insert ";; Welcome h4x0r !!!")
    (newline)
    (insert ";;")
    (newline)))
(init-scratch)

(add-hook 'kill-buffer-query-functions #'dont-kill-scratch)
(defun dont-kill-scratch ()
  (if (not (equal (buffer-name) "*scratch*"))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (init-scratch)
    (bury-buffer)
    nil))

(setenv "PAGER" "cat")

(setq server-inside-emacs-client nil)
; patches the call at server.el:1237
(defun server-create-window-system-frame-hook(orig-fun &rest args)
  (let (
        (expected-display (car-safe args))
        (frame-display (frame-parameter (selected-frame) 'display))
        (other-arguments (cdr-safe args))
        )
  (setq server-inside-emacs-client t)
;  (message "Create server params: %S, frame params: %S" args frame-display)
;  (message "CAR %S is iq %S" expected-display (string-equal (car-safe args) "localhost:current"))
;  (message "CDR %S" other-arguments)
;  (message "REJOIN %S" (cons frame-display other-arguments))
  (if (null frame-display)
      (if (string-equal expected-display "localhost:current")
          (apply orig-fun (cons (getenv "DISPLAY") other-arguments))
        (apply orig-fun args)
        )

    (if (string-equal expected-display "localhost:current")
        (apply orig-fun (cons frame-display other-arguments))
      (apply orig-fun args)
      )
    )
  )

  )

(advice-add 'server-create-window-system-frame :around #'server-create-window-system-frame-hook)


(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(setq CONFIGURATION-PATH (expand-file-name "~/.emacs.d/lisp"))
(setq load-path (cons (expand-file-name "~/.emacs.d/lisp/jss-master") load-path))
(setq load-path (cons CONFIGURATION-PATH load-path))

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(setq custom-theme-load-path (cons (expand-file-name "~/.emacs.d/lisp/themes") custom-theme-load-path))

;; Load the theme
(load-theme 'doom-mhfc t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

(require 'powerline)
(powerline-default-theme)

(require 'cl) ; required for defun*

(global-set-key [(control x) (control c)] (defun dont-kill-emacs() (interactive) (message "Use C-x c to leave")))

(require 'server)
(defun exit-emacs-or-close-frame() (interactive)
       (if server-inside-emacs-client (delete-frame) (save-buffers-kill-emacs)))

(global-set-key [(control x) (c)] 'exit-emacs-or-close-frame)
;(global-set-key [(control a)] 'mark-whole-buffer)
;(global-set-key [(control b)] 'ido-switch-buffer)
;(global-set-key [(control f)] 'find-file)
;(global-set-key [(control o)] 'ido-switch-buffer-other-window)
(global-set-key [(control z)] 'undo)
(global-set-key [f9] 'compile)

(require 'grep)
(defun nice-rgrep() (interactive) (grep-compute-defaults) (rgrep (grep-read-regexp) "*" default-directory nil))
;; (global-set-key [(meta f)] 'nice-rgrep)

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
          (pselect-record (list :path buffer-file-name
                                :line (string-to-number (format-mode-line "%l"))
                                :context (replace-regexp-in-string "\n$" "" (thing-at-point 'line t))))
          )
      (ag--format-result (expand-file-name proj) pselect-record))))

(require 'ivy)
; (push '(counsel-ag . ivy-recompute-index-swiper-async) ivy-index-functions-alist)

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

(global-set-key [(meta f)] 'counsel-ag-preselect)
(global-set-key [f4] 'next-error)
(global-set-key [(shift f4)] 'previous-error)
(global-set-key [f1] 'manual-entry)
(global-set-key [(shift f1)] 'info)
;(global-set-key [(meta right)] 'previous-buffer)
;(global-set-key [(meta left)]  'next-buffer)
;(global-set-key  "\M-`" 'speedbar-get-focus)
(global-set-key [(control f10)] 'start-kbd-macro)
(global-set-key [(meta f10)] 'end-kbd-macro)
(global-set-key [f11] 'kmacro-name-last-macro)
(global-set-key [f12] 'kmacro-call-macro)
(global-set-key [(control f11)] 'name-last-kbd-macro)
(global-set-key [(meta f11)] 'edit-named-kbd-macro)
(global-set-key [(control f12)] 'call-last-kbd-macro)
(global-set-key [(meta f12)] 'edit-last-kbd-macro)
(global-set-key [(control meta b)] 'toggle-truncate-lines)
(global-set-key [(control k)] 'kill-whole-line)
(global-set-key [(control u)] 'yank-rectangle)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [f6] (defun last-buffer() (interactive) (switch-to-buffer (other-buffer))))
(global-set-key [(control tab)] (defun next-buff() (interactive) (other-window 1)))
(global-set-key (kbd "C-S-<iso-lefttab>") (defun prev-buffer() (interactive) (other-window -1)))
(global-set-key [(pause)] 'kill-this-buffer)
(global-set-key (kbd "C-M-k") 'kill-this-buffer)
(global-set-key [(scroll-lock)] 'kill-this-buffer)
;(global-set-key [f8] 'gud-gdb)



(global-set-key [(meta left)] 'backward-sexp)
(global-set-key [(meta right)] 'forward-sexp)
;(global-set-key [(meta shift left)] 'backward-sexp-mark)
;(global-set-key [(meta shift right)] 'forward-sexp-mark)

;(global-set-key [(control meta left)] 'next-buffer)
;(global-set-key [(control meta right)] 'previous-buffer)

(global-set-key (kbd "C-x C-z") (lambda() (interactive) (message "Dont minimize")))

(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<down>") 'windmove-down)

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (when (active-minibuffer-window)
      (if (eq (active-minibuffer-window) (frame-selected-window))
          (select-window (get-mru-window))
          (select-window (active-minibuffer-window))
          )))
(global-set-key (kbd "M-d") 'switch-to-minibuffer)

(require 'redo+)
(global-set-key [(control z)] 'undo)
(global-set-key [(control y)] 'redo)


(require 'comint) ; to set comint-output-filter-functions
(define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
(define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)
(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\|^Password for \\\\'.*\\\\':\\s *\\'"))
(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\|^Password for '.*':\s+"))
(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\|^.*\\\\' password:\\s *\\'"))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'gdb-mode-hook 'ansi-color-for-comint-mode-on)


(require 'shell-ext) ; for f2 to create a new shell window
(defun old-shell-with-dir(current-directory-path)
  (let (
        (shb (car (get-buffers-with-major-mode 'shell-mode)))
        )
    (if (eq shb nil)
        (new-shell-with-dir current-directory-path)
      (switch-to-buffer shb)
      (end-of-buffer)
      (insert-string "cd " )
      (insert-string (shell-quote-argument current-directory-path))
      (comint-send-input))
    shb))
(defun old-shell() (interactive)
  (old-shell-with-dir (expand-file-name default-directory)))
(global-set-key [(meta t)] 'old-shell)
; (global-set-key [(meta \\)] 'shell-change-to-current-dir)
(global-set-key [(meta shift t)] 'new-shell)
; (global-set-key [(meta f2)] 'shell-command-on-region-inplace)

(require 'column-marker)
;(require 'commenting) ; adds my-comment-region, which allows commenting only one line
;(global-set-key [(meta \3)] 'my-comment-region)
;(global-set-key [(meta \4)] 'my-uncomment-region)


(defun lines-in-region () (interactive)
       (message "lines %S" (count-lines (region-beginning) (region-end))))
(global-set-key (kbd "C-;") 'lines-in-region)

(defun my-comment-or-uncomment-region ()
  (interactive)
  (let ((beg (save-excursion (when mark-active (goto-char (region-beginning)))
                             (line-beginning-position)))
	(end (save-excursion (when mark-active (goto-char (region-end)))
                             (line-end-position))))
    (when (and mark-active
               (< beg end)
               (= (save-excursion (goto-char end)
                                  (line-beginning-position)) (region-end)))
      (setq end (- (region-end) 1)))
    (comment-or-uncomment-region beg end 1)))
(global-set-key (kbd "C-/") 'my-comment-or-uncomment-region)

(defun gud-gdb-marker-filter-hook(orig-fun &rest args)
  (let (
        (res (apply orig-fun args))
        )
    (ansi-color-process-output res)
    )
)
(advice-add 'gud-gdb-marker-filter :around #'gud-gdb-marker-filter-hook)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (pcre2el doom-themes powerline ag dumb-jump counsel sr-speedbar persp-mode python-mode swiper company-irony company-anaconda pungi bash-completion multiple-cursors magit-gerrit web-beautify json-mode websocket js-comint web-mode python python-x pyimport elpy bind-key company-web company-irony-c-headers jedi android-mode anaconda-mode company-shell company magit hydra exwm xelb)))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (semantic-mode)
           (setq-local gud-gdb-history
                       (list
                        (concat
                         (file-name-as-directory
                          (locate-dominating-file default-directory dir-locals-file))
                         "src/build/android/adb_gdb_chrome_public " "\"--gdb=/opt/gdb/bin/arm-linux-androideabi-gdb\" " "\"--gdbserver=/home/ubuntu/android/ndk/android-ndk-r10e/prebuilt/android-arm/gdbserver/gdbserver\" " "--interpreter=mi --fullname \"--su-prefix=su -c\" " "--output-directory=out/Default --force  --noautoload " "--no-pull-libs --sandboxed \"--start=http://127.0.0.1:8000/\"")
                        (concat
                         (file-name-as-directory
                          (locate-dominating-file default-directory dir-locals-file))
                         "src/build/android/adb_gdb_chrome_public " "\"--gdb=/opt/gdb/bin/arm-linux-androideabi-gdb\" " "\"--gdbserver=/home/ubuntu/android/ndk/android-ndk-r10e/prebuilt/android-arm/gdbserver/gdbserver\" " "--interpreter=mi --fullname \"--su-prefix=su -c\" " "--output-directory=out/Default --force " "--pull-libs --sandboxed \"--start=http://127.0.0.1:8000/\"")))
           (setq-local compile-command
                       (concat "cd "
                               (file-name-as-directory
                                (locate-dominating-file default-directory dir-locals-file))
                               "src ; " "ninja -C out/Default chrome_public_apk")))
     (eval progn
           (setq-local gud-gdb-history
                       (list
                        (concat
                         (file-name-as-directory
                          (locate-dominating-file default-directory dir-locals-file))
                         "src/build/android/adb_gdb_chrome_public " "\"--gdb=/opt/gdb/bin/arm-linux-androideabi-gdb\" " "\"--gdbserver=/home/ubuntu/android/ndk/android-ndk-r10e/prebuilt/android-arm/gdbserver/gdbserver\" " "--interpreter=mi --fullname \"--su-prefix=su -c\" " "--output-directory=out/Default --force  --noautoload " "--no-pull-libs --sandboxed \"--start=http://127.0.0.1:8000/\"")
                        (concat
                         (file-name-as-directory
                          (locate-dominating-file default-directory dir-locals-file))
                         "src/build/android/adb_gdb_chrome_public " "\"--gdb=/opt/gdb/bin/arm-linux-androideabi-gdb\" " "\"--gdbserver=/home/ubuntu/android/ndk/android-ndk-r10e/prebuilt/android-arm/gdbserver/gdbserver\" " "--interpreter=mi --fullname \"--su-prefix=su -c\" " "--output-directory=out/Default --force " "--pull-libs --sandboxed \"--start=http://127.0.0.1:8000/\"")))
           (setq-local compile-command
                       (concat "cd "
                               (file-name-as-directory
                                (locate-dominating-file default-directory dir-locals-file))
                               "src ; " "ninja -C out/Default chrome_public_apk"))))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))



;(defadvice show-paren-function
;      (after show-matching-paren-offscreen activate)
;      "If the matching paren is offscreen, show the matching line in the
;        echo area. Has no effect if the character before point is not of
;        the syntax class ')'."
;      (interactive)
;      (let* ((cb (char-before (point)))
;             (matching-text (and cb
;                                 (char-equal (char-syntax cb) ?\) )
;                                 (blink-matching-open))))
;        (when matching-text (message matching-text))))

;(setq gdb-command-name "lispgdb")


(defun* mystoppedfun(res)
    ;(message "STOPPED %S" res)
;    (message (format "gdb-thread-num %S gdb-frame-number %S gud running %S, gdb update gud running %S gdb show run p %S gdb show stop %S #running %S #stopped %S" gdb-thread-number gdb-frame-number gud-running (gdb-update-gud-running) (gdb-show-run-p) (gdb-show-stop-p) gdb-running-threads-count gdb-stopped-threads-count))

    (gud-refresh)
    (condition-case nil
    (let (
        (my-frame (bindat-get-field res 'frame))
        )
;            (message "frame %S" my-frame)
            (condition-case nil
                (let(
                    (file (bindat-get-field my-frame 'fullname))
                    (line-number (bindat-get-field my-frame 'line))
                    )
;                        (message "file %S" file)
;                        (message "line is %S" line-number)

                        (let (
                            (my-buffer (find-file-noselect file))
                            )
;                                (message "buffer is %S" my-buffer)
                                (let (
                                    (my-window (display-buffer my-buffer))
                                    )
;                                        (message "window is %S" my-window)
                                        (with-selected-window my-window
                                            (goto-line (string-to-number line-number))
;                                            (message "went to line")
;                                            (set-window-point my-window cur-point)
                                            (winstack-push my-window t)
                                        )

                                )
                        )
                )
            )
    )
    (error nil))
;    (message "done stop handler")

    )

(setq gdb-stopped-functions '(mystoppedfun))

;(let (
;      (gpob (gdb-get-buffer-create 'gdb-partial-output-buffer))
;      )
;  (with-current-buffer gpob
;    (buffer-disable-undo gpob)
;    )
;  )

; (semantic-symref-symbol (symbol-at-point))
; (call-interactively 'ami-cscope-at-point)



;;; make these keys behave like normal browser
;(define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-scroll-down)
;(define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-scroll-up)
;(define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidget-webkit-scroll-down)
;(define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidget-webkit-scroll-up)
;(define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
;(define-key xwidget-webkit-mode-map (kbd "C-c") 'xwidget-webkit-copy-selection-as-kill)

;; adapt webkit according to window configuration chagne automatically
;; without this hook, every time you change your window configuration,
;; you must press 'a' to adapt webkit content to new window size
(add-hook 'window-configuration-change-hook (lambda ()
               (when (equal major-mode 'xwidget-webkit-mode)
                 (xwidget-webkit-adjust-size-dispatch))))


;;; by default, xwidget reuses previous xwidget window,
;;; thus overriding your current website, unless a prefix argument
;;; is supplied
;;;
;;; This function always opens a new website in a new window
;(defun xwidget-browse-url-no-reuse (url &optional sessoin)
;  (interactive (progn
;                 (require 'browse-url)
;                 (browse-url-interactive-arg "xwidget-webkit URL: "
;                                             )))
;  (xwidget-webkit-browse-url url t))

;; make xwidget default browser
(setq browse-url-browser-function (lambda (url session)
                    (other-window 1)
                    (xwidget-webkit-browse-url url)))


(with-eval-after-load 'magit-mode (define-key magit-mode-map [(control tab)] 'other-window))
(setq magit-completing-read-function 'ivy-completing-read)

(setq ivy-use-virtual-buffers t)
(setq ivy-wrap t)
(setq ivy-auto-select-single-candidate t)
(setq ivy-do-completion-in-region nil) ; we have company

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

(require 'swiper)
(define-key ivy-minibuffer-map (kbd "C-d") #'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
(define-key ivy-minibuffer-map [(meta t)] 'ivy-shell)
(define-key ivy-minibuffer-map [(meta shift t)] 'ivy-new-shell)
(define-key ivy-minibuffer-map [(meta p)] 'ivy-python)
(define-key ivy-minibuffer-map [(meta h)] 'ivy-magit-status)
(global-set-key (kbd "M-c") 'ivy-resume)
(global-set-key (kbd "C-s") (lambda () (interactive) (setq swiper--async-direction-backward nil) (swiper-async)))
(global-set-key (kbd "C-r") (lambda () (interactive) (setq swiper--async-direction-backward t) (swiper-async)))
(global-set-key (kbd "C-c C-a") 'mcs-swiper)

;;(global-set-key (kbd "C-/") (lambda () (interactive) (ivy-exit-with-action (lambda (_) (my-comment-or-uncomment-region)))))
(define-key swiper-map (kbd "C-/")
  (lambda () (interactive)
    (with-ivy-window
      (my-comment-or-uncomment-region)
      (ivy--exhibit))))

(define-key swiper-map (kbd "C-k")
  (lambda () (interactive)
    (with-ivy-window
      (kill-whole-line)
      (ivy--exhibit))))

(define-key swiper-map (kbd "M-f")
  (lambda () (interactive)
    (ivy-quit-and-run
      (counsel-ag-preselect ivy-text))))

(define-key ivy-minibuffer-map (kbd "M-f")
  (lambda () (interactive)
    (ivy-quit-and-run
      (counsel-ag-preselect ivy-text))))

(defun swiper--goto-original-point()
  (interactive)
  (with-ivy-window
    (setq swiper--current-match-start swiper--opoint)
    (setq swiper--current-line (string-to-number (format-mode-line "%l")))
    (goto-char swiper--opoint)
    ))

(require 'multiple-cursors-swiper)

(define-key swiper-map (kbd "C-g") 'mcs-minibuffer-keyboard-quit)
(define-key swiper-map (kbd "C-SPC") 'mcs-toggle-cursor-at-point)
(define-key swiper-map (kbd "C->") 'mcs-mark-next-like-this)
(define-key swiper-map (kbd "C-<") 'mcs-mark-previous-like-this)
(define-key swiper-map (kbd "C-<up>") 'ivy-previous-line)
(define-key swiper-map (kbd "C-<down>") 'ivy-next-line)
(define-key swiper-map (kbd "M-r") 'ivy-rotate-preferred-builders)

(define-key swiper-map (kbd "M-C-p") 'swiper--goto-original-point)

(require 'counsel)
(define-key counsel-find-file-map (kbd "C-o") (lambda () (interactive) (counsel-find-file-as-root ivy-text)))


(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-a") 'counsel-locate)
(defun ivy-previous-line-or-history-2 (arg)
  "Move cursor vertically up ARG candidates.
If the input is empty, select the previous history element instead."
  (interactive "p")
  (when (string= ivy-text "")
    (ivy-previous-history-element 1))
  (ivy-previous-line arg))
(define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line-or-history-2)
(define-key ivy-minibuffer-map (kbd "C-d") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)

(defun my-toggle-truncate-lines ()
  "Toggle truncate lines in quietly."
  (interactive)
  (let ((inhibit-message t))
    (toggle-truncate-lines)))
(define-key ivy-minibuffer-map (kbd "C-l") 'my-toggle-truncate-lines)
(define-key swiper-map (kbd "C-l") 'my-toggle-truncate-lines)
(global-set-key (kbd "C-l") 'my-toggle-truncate-lines)

(require 'counsel)
(define-key counsel-ag-map (kbd "<down>") (lambda() (interactive) (ivy-next-line-and-call)))
(define-key counsel-ag-map (kbd "<up>") (lambda() (interactive) (ivy-previous-line-and-call)))
(define-key counsel-ag-map (kbd "C-<up>") 'ivy-previous-line)
(define-key counsel-ag-map (kbd "C-<down>") 'ivy-next-line)

; (defvar ivy-grep-map
;   (let ((map (make-sparse-keymap)))
;     (define-key map (kbd "C-d") 'ivy-occur)
;     map)
;   "Keymap used in the minibuffer.")
; (defun ivy-grep ()
;   (interactive)
;   (unless (featurep 'ivy)
;     (require 'ivy))
;   (let* ((keyword (read-string "Keyword:")))
;     (ivy-read
;      (format "Grep at %s:" default-directory)
;      (split-string (shell-command-to-string (format "grep -rsnI %s ." keyword)) "\n" t)
;      :keymap ivy-grep-map
;      :action (lambda (val)
;                (let* ((lst (split-string val ":"))
;                       (linenum (string-to-number (cadr lst))))
;                  ;; open file
;                  (find-file (car lst))
;                  ;; goto line if line number exists
;                  (when (and linenum (> linenum 0))
;                    (goto-char (point-min))
;                    (forward-line (1- linenum))))))))

; (global-set-key [(meta f)] 'ivy-grep)

(require 'swiper-async)

; (defun complete-or-indent ()
;     (interactive)
;     (if (company-manual-begin)
;         (company-complete-common)
;       (indent-according-to-mode)))
; ;(global-set-key [(tab)] 'complete-or-indent)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-SPC") 'company-complete)

(require 'company-irony)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(require 'company-py-shell)

;;(require 'cc-mode)
;;(define-key c-mode-map  [(tab)] 'company-complete)
;;(define-key c++-mode-map  [(tab)] 'company-complete)

;; (require 'shell)
;; (define-key shell-mode-map [(tab)] 'company-complete)

;(global-set-key [(f2)] 'gud-break)

(require 'completion-epc) ; required for frida completion
(require 'ahg) ; for mercurial source control, like magit
;(global-set-key [(meta h)] 'ahg-status)
(global-set-key [(meta h)] 'magit-status)

;(require 'ascope)
;(global-set-key [(meta f9)] 'ascope-find-this-text-stringy)
;(global-set-key [(control f3)] 'ascope-find-global-definition)
;(global-set-key [(control f4)] 'ascope-find-this-symbol)
; (global-set-key [(control f5)] 'ascope-find-functions-calling-this-function)
; (global-set-key [(control f6)] 'ascope-find-called-functions)
; (global-set-key [(control f7)] 'ascope-find-files-including-file)
; (global-set-key [(control f8)] 'ascope-find-all-symbol-assignments)
;(ascope-init "/home/ubuntu/sources/chromium/")

(require 'jss) ; remote js debugger
(require 'winstack)
(require 'simple)
(global-set-key [(control meta p)] 'winstack-pop)
(global-set-key [(control meta n)] 'winstack-next)


; clipboard useful shortcuts
(global-set-key [(control shift v)] 'x-clipboard-yank)
(global-set-key [(control shift x)] 'clipboard-kill-region)
(global-set-key [(control shift c)] 'clipboard-kill-ring-save)

(defun lunaryorn-new-buffer-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (switch-to-buffer buffer)))

(global-set-key [(control n)] 'lunaryorn-new-buffer-frame)


(require 'lk-file-search)

;(require 'webkit)

(require 'python-mode)
; switch to the interpreter after executing code
(setq py-split-window-on-execute nil)
(setq py-switch-buffers-on-execute-p nil)
(setq py-shell-prompt-read-only nil)
; don't split windows
(py-split-window-on-execute-off)
; try to automagically figure out indentation
;(setq py-smart-indentation t)


(defun py--send-string-return-output (strg &optional process msg)
  "Send STRING to PROCESS and return output.

When MSG is non-nil messages the first line of STRING.  Return
the output."
  (let ((process (or process (get-buffer-process (py-shell)))))
    (with-current-buffer (process-buffer process)
      (let* ((erg "")
	     (comint-preoutput-filter-functions
	      (append comint-preoutput-filter-functions
		      '(ansi-color-filter-apply
			(lambda (strg)
			  (progn (setq erg (concat erg strg)) ""))))))
	(py-send-string strg process)
	(accept-process-output process 5)
	(sit-for 0.1 t)
	(when (and erg (not (string= "" erg)))
	  (setq erg
		(replace-regexp-in-string
		 (format "[ \n]*%s[ \n]*" py-fast-filter-re)
		 "" erg)))
	;; (sit-for 0.1 t)
	erg))))



(put 'erase-buffer 'disabled nil)


(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
     major-mode))

(defun get-buffers-with-major-mode (find-major-mode)
  "Get a list of buffers in which minor-mode is active"
  (interactive)
  (let ((major-mode-buffers))
    (dolist (buf (buffer-list) major-mode-buffers)
      (with-current-buffer buf
        (when (eq major-mode find-major-mode)
          (push buf major-mode-buffers))))))

(defun new-python-in-dir(dir) (interactive)
  (let (
        (b (generate-new-buffer "*python*"))
        )
    (switch-to-buffer b)
    (setq default-directory dir)
    (python :fast nil :buffer (buffer-name b) :shell (py-choose-shell nil nil))
    (let (
          (proc (get-buffer-process (get-buffer b)))
          )
      (py-send-string py-shell-completion-setup-code proc))))

(defun new-python() (interactive)
       (let (
              (buffer (python :fast nil))
              )
         (let (
               (proc (get-buffer-process (get-buffer buffer)))
               )
           (py-send-string py-shell-completion-setup-code proc))
         (switch-to-buffer buffer)))
(defun old-python() (interactive)
       (let (
             (pyb (car (get-buffers-with-major-mode 'py-python-shell-mode)))
             )
         (if (eq pyb nil)
             (new-python)
           (switch-to-buffer pyb))))
(global-set-key [(meta p)] 'old-python)
(global-set-key [(meta shift p)] 'new-python)
(define-key shell-mode-map [(meta p)] 'old-python)
(define-key shell-mode-map [(meta shift p)] 'new-python)

(defun track-shell-directory/procfs ()
  (shell-dirtrack-mode 0)
  (add-hook 'comint-preoutput-filter-functions
            (lambda (str)
              (prog1 str
                (when (string-match comint-prompt-regexp str)
                  (cd (file-symlink-p
                       (format "/proc/%s/cwd" (process-id
                                               (get-buffer-process
                                                (current-buffer)))))))))
            nil t))

(add-hook 'shell-mode-hook 'track-shell-directory/procfs)

(require 'multiple-cursors)
(require 'multiple-cursors-yank)
(require 'multiple-cursors-sync-window)
; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-<f3>") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-l") 'mc/edit-ends-of-lines)


(require 'jedi-core)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional
(define-key jedi-mode-map (kbd "M-C-j") 'jedi:goto-definition)

(require 'bash-completion)
(bash-completion-setup)

(setq helm-alive-p nil) ; fix sr-speedbur bug
(require 'sr-speedbar)
(sr-speedbar-refresh-turn-off)
(defun sr-speedbar-find-window-buffer()
  (setq sr-speedbar-window (get-buffer-window sr-speedbar-buffer-name 'visible))
  (if sr-speedbar-window
      (setq speedbar-buffer (window-buffer sr-speedbar-window))
      (setq speedbar-buffer nil)))

(setq sr-speedbar-right-side t)

(defun sr-speedbar-get-window ()
  "Get `sr-speedbar' window."
  (let (
        (current-window (frame-root-window))
        )
    (let (
          ;; Get split new window.
          (new-window (split-window
                       current-window
                       (if sr-speedbar-right-side
                           (- (sr-speedbar-current-window-take-width current-window) sr-speedbar-width)
                         sr-speedbar-width)
                       t))
          )
    ;; Select split window.
      (setq sr-speedbar-window
            (if sr-speedbar-right-side
                ;; Select right window when `sr-speedbar-right-side' is enable.
                new-window
              ;; Otherwise select left widnow.
              current-window)))))

(defun sr-speedbar-exist-p-hook(orig-fun &rest args)
  (sr-speedbar-find-window-buffer)
  (apply orig-fun args)
  ;(get-buffer-window sr-speedbar-buffer-name 'visible)
  )
(advice-add 'sr-speedbar-exist-p :around #'sr-speedbar-exist-p-hook)

(defun sr-speedbar-navigate() (interactive)
  (let (
        (root-dir (read-directory-name "Navigate Speedbar: "))
        )
    (with-temp-buffer
      (setq default-directory root-dir)
      (speedbar-refresh))
    root-dir))

(add-hook 'speedbar-reconfigure-keymaps-hook
          '(lambda ()
             (define-key speedbar-mode-map (kbd "<backspace>") 'speedbar-up-directory)
             (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
             (define-key speedbar-mode-map [left] 'speedbar-contract-line)
             (define-key speedbar-mode-map [M-up] 'speedbar-restricted-prev)
             (define-key speedbar-mode-map [M-down] 'speedbar-restricted-next)
             (define-key speedbar-mode-map [up] 'speedbar-prev)
             (define-key speedbar-mode-map [down] 'speedbar-next)
             (define-key speedbar-mode-map (kbd "M-g") 'sr-speedbar-navigate)
             (define-key speedbar-mode-map [(control meta p)] 'winstack-pop)
             (define-key speedbar-mode-map [(control meta n)] 'winstack-next)
             ))

(setq speedbar-last-window nil)
(defun sr-speedbar-toggle-keep-window ()
  "Toggle sr-speedbar window.
Toggle visibility of sr-speedbar by resizing
the `sr-speedbar-window' to a minimal width
or the last width when visible.
Use this function to create or toggle visibility
of a speedbar-window.  It will be created if necessary."
  (interactive)
  (if (sr-speedbar-exist-p)
      (progn
        (sr-speedbar-close)
        (when
            (window-live-p speedbar-last-window)
          (select-window speedbar-last-window))
        (setq speedbar-last-window nil))
    (progn
      (sr-speedbar-open)
      (setq speedbar-last-window (frame-selected-window))
      (sr-speedbar-select-window))))
(global-set-key (kbd "C-e") 'sr-speedbar-toggle-keep-window)


(require 'persp-mode)

; (defun* persp-def-buffer-save/load
;     (&rest
;      keyargs
;      &key buffer-name file-name mode mode-name minor-mode minor-mode-name
;      predicate tag-symbol save-vars save-function load-function after-load-function
;      mode-restore-function
;      append)
;   (let ((generated-save-predicate
;          (apply #'persp--generate-buffer-predicate keyargs))
;         save-body load-fun)
;     (when save-vars
;       (unless (listp save-vars) (setq save-vars (list save-vars)))
;       (when (and (or mode mode-name) (not (memq 'major-mode save-vars)))
;         (push 'major-mode save-vars)))
;     (unless tag-symbol (setq tag-symbol 'def-buffer-with-vars))

;     (setq save-body
;           `(let ((vars-list
;                   (with-current-buffer buffer
;                     (delete-if-not
;                      #'(lambda (lvar)
;                          (and
;                           ,(persp--generate-predicate-loop-any-all
;                             save-vars
;                             '(if (persp-regexp-p item)
;                                  (persp-string-match-p item
;                                                        (symbol-name lvar))
;                                (eq item lvar))
;                             t)
;                           (persp-elisp-object-readable-p
;                            (symbol-value lvar))))
;                      (buffer-local-variables)
;                      :key #'car-safe))))
;              ,(if save-function
;                   `(funcall (with-no-warnings ',save-function)
;                             buffer ',tag-symbol vars-list)
;                 `(list ',tag-symbol (buffer-name buffer) vars-list)))
;           save-body `(when (funcall (with-no-warnings ',generated-save-predicate)
;                                     buffer)
;                        ,save-body))

;     (setq load-fun
;           `(lambda (savelist)
;              (destructuring-bind
;                  (buffer-name vars-list &rest _rest) (cdr savelist)
;                (let ((buf-file (alist-get 'buffer-file-name vars-list))
;                      (buf-mmode (alist-get 'major-mode vars-list)))
;                  ,(when mode-restore-function
;                     `(push (cons 'persp-load-buffer-mode-restore-function
;                                  (with-no-warnings ',mode-restore-function))
;                            vars-list))
;                  (lexical-let
;                      ((persp-loaded-buffer
;                        (persp-buffer-from-savelist
;                         (list 'def-buffer buffer-name buf-file buf-mmode
;                               (list (cons 'local-vars vars-list)))))
;                       (persp-after-load-function (with-no-warnings
;                                                    ',after-load-function))
;                       persp-after-load-lambda)
;                    (when (and persp-loaded-buffer persp-after-load-function)
;                      (setq persp-after-load-lambda
;                            #'(lambda (&rest pall-args)
;                                (apply persp-after-load-function
;                                       persp-loaded-buffer pall-args)
;                                (remove-hook 'persp-after-load-state-functions
;                                             persp-after-load-lambda)))
;                      (add-hook 'persp-after-load-state-functions
;                                persp-after-load-lambda t))
;                    persp-loaded-buffer)))))

;     (add-hook 'persp-save-buffer-functions
;               (eval `(lambda (buffer) ,save-body)) append)
;     (add-hook 'persp-load-buffer-functions
;               (eval
;                `(lambda (savelist)
;                   (when (eq (car savelist) ',tag-symbol)
;                     (let ((default-load-fun (with-no-warnings ',load-fun)))
;                       ,(if load-function
;                            `(funcall (with-no-warnings ',load-function)
;                                      savelist default-load-fun
;                                      (with-no-warnings ',after-load-function))
;                          `(funcall default-load-fun savelist))))))
;               append)))


(global-set-key (kbd "M-1") (defun perspsw1() (interactive) (persp-switch "1")))
(global-set-key (kbd "M-2") (defun perspsw2() (interactive) (persp-switch "2")))
(global-set-key (kbd "M-3") (defun perspsw3() (interactive) (persp-switch "3")))
(global-set-key (kbd "M-4") (defun perspsw4() (interactive) (persp-switch "4")))
(global-set-key (kbd "M-5") (defun perspsw5() (interactive) (persp-switch "5")))
(global-set-key (kbd "M-6") (defun perspsw6() (interactive) (persp-switch "6")))
(global-set-key (kbd "M-7") (defun perspsw7() (interactive) (persp-switch "7")))
(global-set-key (kbd "M-8") (defun perspsw8() (interactive) (persp-switch "8")))
(global-set-key (kbd "M-9") (defun perspsw9() (interactive) (persp-switch "9")))
(global-set-key (kbd "M-0") (defun perspsw0() (interactive) (persp-switch "0")))

;(setq persp-autokill-buffer-on-remove nil)
(setq persp-auto-resume-time 0.1)
(setq persp-auto-save-fname "autosave")
(setq persp-auto-save-opt 1)
(setq persp-nil-hidden t)
(setq persp-nil-name "nil")
(setq persp-add-buffer-on-after-change-major-mode 'free)

; (with-eval-after-load "persp"
;   (setq persp-filter-save-buffers-functions (list #'(lambda (b) nil)))
;   (add-to-list 'persp-save-buffer-functions
;                #'(lambda (b)
;                    (when (eq 'inferior-python-mode (buffer-local-value 'major-mode b))
;                      `(def-inferior-python-buffer ,(buffer-name b)
;                         ,(let ((process (get-buffer-process b)))
;                            (if process
;                                (progn
;                                  (python-shell-send-string "import os" process)
;                                  (python-shell-send-string-no-output "os.getcwd()" process))
;                              (concat "'" (buffer-local-value 'default-directory b) "'")))))))
;   (add-to-list 'persp-load-buffer-functions
;                #'(lambda (savelist)
;                    (when (eq (car savelist) 'def-inferior-python-buffer)
;                      (destructuring-bind (bname dir) (cdr savelist)
;                        (run-python nil nil nil)
;                        (with-current-buffer (python-shell-get-buffer)
;                          (rename-buffer bname)
;                          (cd dir)
;                          (python-shell-send-string "import os")
;                          (python-shell-send-string (format "os.chdir(%s)" dir))
;                          (current-buffer))))))



;   )

; (persp-def-buffer-save/load
;  :mode 'shell-mode
;  :mode-restore-function #'(lambda (_mode) (shell)) ; or #'identity if you do not want to start shell process
;  :tag-symbol 'def-shell
;  :save-vars '(major-mode default-directory))

; ;; eshell
; (persp-def-buffer-save/load
;  :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
;  :save-vars '(major-mode default-directory))

; ;; compile
; (persp-def-buffer-save/load
;  :mode 'compilation-mode :tag-symbol 'def-compilation-buffer
;  :save-vars '(major-mode default-directory compilation-directory
;                          compilation-environment compilation-arguments))

; ;; magit-status
; (with-eval-after-load "magit-autoloads"
;   (autoload 'magit-status-mode "magit")
;   (autoload 'magit-refresh "magit")
;   (persp-def-buffer-save/load
;    :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
;    :save-vars '(major-mode default-directory)
;    :after-load-function #'(lambda (b &rest _)
;                                   (with-current-buffer b (magit-refresh)))))


; (setq persp-shared-buffers '("*scratch*" "*Messages*" "*Backtrace*"))
; (add-hook 'persp-activated-functions
;           #'(lambda (_)
;               (persp-add-buffer persp-shared-buffers)))

(with-eval-after-load "persp-mode"
  (setq wg-morph-on nil)
  (set-persp-parameter 'dont-save-to-file t nil)
  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))

    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil)))))

  ;; switch off the animation of restoring window configuration
  (add-hook 'after-init-hook
            #'(lambda ()
                (progn
                  (persp-mode 1)
                  (run-at-time "1 sec" nil 'perspsw1)
                  )))
  )

(add-to-list 'speedbar-frame-parameters (cons 'persp-ignore-wconf t))

(require 'dumb-jump)
(global-set-key (kbd "M-C-j") 'dumb-jump-go)
(global-set-key (kbd "M-C-q") 'dumb-jump-quick-look)

(defun dumb-jump-ivy-minibuffer-keyboard-quit () (interactive)
  (with-ivy-window
    (goto-char dumb-jump--opoint))
  (minibuffer-keyboard-quit))

(defun dumb-jump--goto-original-point()
  (interactive)
  (with-ivy-window
    (setq swiper--current-match-start dumb-jump--opoint)
    (setq swiper--current-line (string-to-number (format-mode-line "%l")))
    (goto-char dumb-jump--opoint)
    ))

(setq dumb-jump-last-query nil)
(defalias 'dumb-jump-populate-regexes (lambda (look-for regexes variant)
  "Take list of REGEXES and populate the LOOK-FOR target and return that list."
  (setq dumb-jump-last-query look-for)
  (--map (dumb-jump-populate-regex it look-for variant) regexes)))

(defvar dumb-jump-ivy-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down>") (lambda() (interactive) (ivy-next-line-and-call)))
    (define-key map (kbd "<up>") (lambda() (interactive) (ivy-previous-line-and-call)))
    (define-key map (kbd "C-<up>") 'ivy-previous-line)
    (define-key map (kbd "C-<down>") 'ivy-next-line)
    (define-key map (kbd "M-f")
      (lambda () (interactive)
        (ivy-quit-and-run
          (counsel-ag-preselect dumb-jump-last-query)
          )))
    (define-key map (kbd "C-l") 'ivy-call-and-recenter)
    (define-key map (kbd "M-C-p") 'dumb-jump--goto-original-point)
    (define-key map (kbd "M-C-n") (lambda () (interactive)))
    (define-key map (kbd "C-g") 'dumb-jump-ivy-minibuffer-keyboard-quit)
    map))

(ivy-set-display-transformer 'dumb-jump-ivy-jump-to-selected 'counsel-git-grep-transformer)


(defun dumb-jump-ivy-jump-to-selected-with-call (results choices proj)
  "Offer CHOICES as canidates through ivy-read then execute
dumb-jump-to-selected on RESULTS CHOICES and selected choice.
Ignore PROJ"
  (setq dumb-jump--opoint (point))
  (let (
        (pselect-record (list :path (file-name-nondirectory buffer-file-name)
                              :line (string-to-number (format-mode-line "%l"))
                              :context (thing-at-point 'line t)))
        )
      (unwind-protect
          (let (
                (res (ivy-read "Jump to: " choices
                               :preselect (dumb-jump--format-result proj pselect-record)
                               :keymap dumb-jump-ivy-map
                               ;;:unwind #'swiper--cleanup
                               :caller 'dumb-jump-ivy-jump-to-selected
                               :action (lambda (x) (dumb-jump-to-selected results choices x))
                               ;;   :require-match t
                               :history 'swiper-history
                               ))
                )
            (unless res
              (goto-char dumb-jump--opoint))
            (unless (or res (string= ivy-text ""))
              (cl-pushnew ivy-text swiper-history))
                                        ; (when swiper--reveal-mode
                                        ; (reveal-mode 1))
            ))))

(defalias 'dumb-jump-ivy-jump-to-selected 'dumb-jump-ivy-jump-to-selected-with-call)

(setq dumb-jump-selector 'ivy)


(advice-add 'ivy-read :around #'wrap-winstack-hook)
(advice-add 'switch-to-buffer :around #'wrap-winstack-hook)
(advice-add 'other-window :around #'wrap-winstack-hook)
(advice-add 'select-window :around #'wrap-winstack-hook)
(advice-add 'quit-window :around #'wrap-winstack-hook)
(advice-add 'windmove-left :around #'wrap-winstack-hook)
(advice-add 'windmove-right :around #'wrap-winstack-hook)
(advice-add 'windmove-up :around #'wrap-winstack-hook)
(advice-add 'windmove-down :around #'wrap-winstack-hook)
(advice-add 'goto-line :around #'wrap-winstack-hook)
; (advice-add 'goto-char :around #'wrap-winstack-hook)

(advice-add 'dumb-jump-goto-file-line :around #'wrap-winstack-hook)
(advice-add 'jedi:goto-definition--nth :around #'wrap-winstack-hook)

	
(which-function-mode 1)

(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (cond
   ((eq (car data) 'beginning-of-buffer) (goto-char (point-min)))
   ((eq (car data) 'end-of-buffer) (goto-char (point-max)))
   (t (command-error-default-function data context caller))))
(setq command-error-function #'my-command-error-function)
