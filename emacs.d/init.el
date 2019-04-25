
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'package)

(setq enable-local-eval nil)
(setq enable-local-variables nil)

;; for 64 bit systems
;; https://emacs.stackexchange.com/questions/3824/what-piece-of-code-in-emacs-makes-line-number-mode-print-as-line-number-i/3827#3827
(setq line-number-display-limit-width 2000000)

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

(setenv "PAGER" "cat")

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(setq CONFIGURATION-PATH (expand-file-name "~/.emacs.d/lisp"))
(setq load-path (cons (expand-file-name "~/.emacs.d/lisp/jss-master") load-path))
(setq load-path (cons CONFIGURATION-PATH load-path))

(require 'server-hook)
(require 'scratch-util)
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
(require 'utils)

(global-set-key [(control x) (control c)] 'dont-kill-emacs)

(require 'server)

(global-set-key [(control x) (c)] 'exit-emacs-or-close-frame)
(global-set-key [(control z)] 'undo)
(global-set-key [f9] 'compile)

(require 'counsel-ag-ext)

(defvar next-key (kbd "C-M-n"))
(defvar pop-key (kbd "C-M-p"))
(defvar goto-def-key (kbd "C-M-g"))
(defvar sc-status-key (kbd "M-h"))

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
(global-set-key [(control k)] 'kill-whole-line)
(global-set-key [(control u)] 'yank-rectangle)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [f6] 'last-buffer)
(global-set-key [(control tab)] 'next-buff)
(global-set-key (kbd "C-S-<iso-lefttab>") 'prev-buffer)
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

(global-set-key (kbd "M-d") 'switch-to-minibuffer)

(require 'redo+)
(global-set-key [(control z)] 'undo)
(global-set-key [(control y)] 'redo)


(require 'comint) ; to set comint-output-filter-functions
(define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
(define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)

(require 'comint-ext)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'gdb-mode-hook 'ansi-color-for-comint-mode-on)


(require 'shell-ext) ; for f2 to create a new shell window

(global-set-key [(meta t)] 'old-shell)
; (global-set-key [(meta \\)] 'shell-change-to-current-dir)
(global-set-key [(meta shift t)] 'new-shell)
; (global-set-key [(meta f2)] 'shell-command-on-region-inplace)

(require 'column-marker)
;(require 'commenting) ; adds my-comment-region, which allows commenting only one line
;(global-set-key [(meta \3)] 'my-comment-region)
;(global-set-key [(meta \4)] 'my-uncomment-region)

(global-set-key (kbd "C-;") 'lines-in-region)
(global-set-key (kbd "C-/") 'my-comment-or-uncomment-region)

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



; (semantic-symref-symbol (symbol-at-point))
; (call-interactively 'ami-cscope-at-point)

(require 'xwidget-ext)

(with-eval-after-load 'magit-mode
  (define-key magit-mode-map [(control tab)] 'other-window))
(setq magit-completing-read-function 'ivy-completing-read)

(setq ivy-use-virtual-buffers t)
(setq ivy-wrap t)
(setq ivy-auto-select-single-candidate t)
(setq ivy-do-completion-in-region nil) ; we have company

;(setq ivy-magic-slash-non-match-action nil)

(require 'ivy-utils)

(require 'swiper)
(define-key ivy-minibuffer-map (kbd "C-d") #'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
(define-key ivy-minibuffer-map [(meta t)] 'ivy-shell)
(define-key ivy-minibuffer-map [(meta shift t)] 'ivy-new-shell)
(define-key ivy-minibuffer-map [(meta p)] 'ivy-python)
(define-key ivy-minibuffer-map sc-status-key 'ivy-magit-status)
(global-set-key (kbd "M-c") 'ivy-resume)

(require 'swiper-async)
(global-set-key (kbd "C-s") 'swiper-async-search-forward)
(global-set-key (kbd "C-r") 'swiper-async-search-backward)
(global-set-key (kbd "C-c C-a") 'mcs-swiper)

(setq swiper-include-line-number-in-search t)

(define-key swiper-map (kbd "C-/") 'swiper-comment-or-uncomment-line)
(define-key swiper-map (kbd "C-k") 'swiper-kill-line)
(define-key swiper-map (kbd "M-f") 'swiper-convert-to-ag)
(define-key ivy-minibuffer-map (kbd "M-f") 'swiper-convert-to-ag)

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
(define-key counsel-find-file-map (kbd "C-o") 'ivy-find-file-as-root)


(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-a") 'counsel-locate)
(global-set-key (kbd "C-x p") 'counsel-git)

(define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line-or-history-2)
(define-key ivy-minibuffer-map (kbd "C-d") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)

(define-key ivy-minibuffer-map (kbd "C-l") 'my-toggle-truncate-lines)
(define-key swiper-map (kbd "C-l") 'my-toggle-truncate-lines)
(global-set-key (kbd "C-l") 'my-toggle-truncate-lines)

(require 'counsel)
(define-key counsel-ag-map (kbd "<down>") 'ivy-next-line-and-call)
(define-key counsel-ag-map (kbd "<up>") 'ivy-previous-line-and-call)
(define-key counsel-ag-map (kbd "C-<up>") 'ivy-previous-line)
(define-key counsel-ag-map (kbd "C-<down>") 'ivy-next-line)

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
;(global-set-key sc-status-key 'ahg-status)
(global-set-key sc-status-key 'magit-status)

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
(global-set-key pop-key 'winstack-pop)
(global-set-key next-key 'winstack-next)


; clipboard useful shortcuts
(global-set-key [(control shift v)] 'x-clipboard-yank)
(global-set-key [(control shift x)] 'clipboard-kill-region)
(global-set-key [(control shift c)] 'clipboard-kill-ring-save)

(global-set-key [(control n)] 'new-buffer-frame)

;(require 'webkit)

(require 'python-mode)
(require 'python-ext)

(global-set-key [(meta p)] 'old-python)
(global-set-key [(meta shift p)] 'new-python)
(define-key shell-mode-map [(meta p)] 'old-python)
(define-key shell-mode-map [(meta shift p)] 'new-python)

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
(define-key jedi-mode-map goto-def-key 'jedi:goto-definition)

(require 'bash-completion)
(bash-completion-setup)

(require 'sr-speedbar-ext)

(require 'persp-mode)

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

(require 'persp-mode-ext)

(require 'dumb-jump)
(global-set-key goto-def-key 'dumb-jump-go)

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

(add-hook 'dumb-jump-after-jump-hook 'swiper--async-which-func-update)
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
                               :action (lambda (x)
                                         (dumb-jump-to-selected results choices x))
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


; (advice-add 'ivy-read :around #'wrap-winstack-hook)
(advice-add 'switch-to-buffer :around #'wrap-winstack-hook)
; (advice-add 'other-window :around #'wrap-winstack-hook)
; (advice-add 'select-window :around #'wrap-winstack-hook)
; (advice-add 'quit-window :around #'wrap-winstack-hook)
; (advice-add 'windmove-left :around #'wrap-winstack-hook)
; (advice-add 'windmove-right :around #'wrap-winstack-hook)
; (advice-add 'windmove-up :around #'wrap-winstack-hook)
; (advice-add 'windmove-down :around #'wrap-winstack-hook)
(advice-add 'goto-line :around #'wrap-winstack-hook)
; (advice-add 'goto-char :around #'wrap-winstack-hook)

(advice-add 'dumb-jump-goto-file-line :around #'wrap-winstack-hook)
(advice-add 'jedi:goto-definition--nth :around #'wrap-winstack-hook)
(advice-add 'swiper-async :around #'wrap-winstack-hook)
(advice-add 'swiper :around #'wrap-winstack-hook)
(advice-add 'mcs-swiper :around #'wrap-winstack-hook)
(advice-add 'counsel-ag :around #'wrap-winstack-hook)
(advice-add 'find-file :around #'wrap-winstack-hook)

(advice-add 'persp-asave-on-exit :around #'disable-winstack-hook)
	
(which-function-mode 1)

(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (cond
   ((eq (car data) 'beginning-of-buffer) (goto-char (point-min)))
   ((eq (car data) 'end-of-buffer) (goto-char (point-max)))
   (t (command-error-default-function data context caller))))
(setq command-error-function #'my-command-error-function)
