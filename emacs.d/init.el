;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; (toggle-debug-on-error)
(setq enable-local-eval nil)
(setq enable-local-variables nil)

;; for 64 bit systems
;; https://emacs.stackexchange.com/questions/3824/what-piece-of-code-in-emacs-makes-line-number-mode-print-as-line-number-i/3827#3827
(setq line-number-display-limit-width 2000000)
(setq max-specpdl-size 10240)

(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")  ;; definitions from...
(setq save-abbrevs 'silent)        ;; save abbrevs when files are saved

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foundry "adobe" :slant normal :weight normal :height 94 :width normal)))))

(setq CONFIGURATION-PATH (expand-file-name "~/.emacs.d/lisp"))
(setq load-path (cons (expand-file-name (concat CONFIGURATION-PATH "/jss-master")) load-path))
(setq load-path (cons CONFIGURATION-PATH load-path))

;; (set-cursor-color "white")
;; (set-mouse-color "white")

(setq helm-alive-p nil) ; fix sr-speedbur bug

(tool-bar-mode -1)
(setq inhibit-splash-screen t)

;; Prevent the annoying beep on errors
(setq visible-bell t)

(blink-cursor-mode -1)
(delete-selection-mode 1)

(global-eldoc-mode -1)

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

(ido-mode) ;; before ivy is installed
(fset 'yes-or-no-p 'y-or-n-p)

(setq global-mark-ring-max 1000)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 1) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq frame-background-mode 'dark)
(setq custom-theme-load-path (cons (expand-file-name "~/.emacs.d/lisp/themes") custom-theme-load-path))
(load-theme 'normal-black t)

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" "emacsclient")

(require 'server-hook)

(find-file (expand-file-name "~/.emacs.d/server.log"))
(setq server-buffer (get-file-buffer "~/.emacs.d/server.log"))
(with-current-buffer server-buffer
  (set-buffer-auto-saved))
(setq server-log t)

(require 'scratch-util)

(require 'cl) ; required for defun*
(require 'utils)

(global-set-key [(control x) (control c)] 'dont-kill-emacs)

(global-set-key [(control x) (c)] 'exit-emacs-or-close-frame)
(global-set-key [(control z)] 'undo)
(global-set-key [f9] 'compile)
(global-set-key (kbd "C-l") 'my-toggle-truncate-lines)


(defvar next-key (kbd "C-M-n"))
(defvar pop-key (kbd "C-M-p"))
(defvar goto-def-key (kbd "C-M-g"))
(defvar find-ref-key (kbd "C-M-x"))
(defvar complete-key (kbd "C-SPC"))
(defvar sc-status-key (kbd "M-h"))

(global-set-key [f4] 'next-error)
(global-set-key [(shift f4)] 'previous-error)
(global-set-key [f1] 'manual-entry)
(global-set-key [(shift f1)] 'info)
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

(global-set-key [(meta left)] 'backward-sexp)
(global-set-key [(meta right)] 'forward-sexp)

(global-set-key (kbd "C-x C-z") (lambda() (interactive) (message "Dont minimize")))


(global-set-key [(meta p)] 'run-python)
(global-set-key [(meta shift p)] 'run-python)

;; ;; window resize (alt+shift)
;; (global-set-key [(meta shift left)] 'shrink-window-horizontally)
;; (global-set-key [(meta shift right)] 'enlarge-window-horizontally)
;; (global-set-key [(meta shift down)] 'enlarge-window)
;; (global-set-key [(meta shift up)] 'shrink-window)

(require 'shell)
(define-key shell-mode-map [(meta p)] 'run-python)
(define-key shell-mode-map [(meta shift p)] 'run-python)
(define-key shell-mode-map [(shift return)] 'newline)

(require 'windmove)
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
(define-key comint-mode-map (kbd "M-r") 'counsel-shell-history)
(define-key comint-mode-map [(shift return)] 'newline)
(define-key comint-mode-map (kbd "C-c C-]") (lambda () (interactive) (insert (kbd "C-]")) (comint-send-input)))

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
(global-set-key (kbd "C-;") 'lines-in-region)
(global-set-key (kbd "C-/") 'my-comment-or-uncomment-region)

(require 'lockstep)
(unless server-inside-emacs-client
  (lockstep))

(require 'term)
(define-key term-mode-map [(control tab)] 'next-buff)
(define-key term-mode-map (kbd "C-S-<iso-lefttab>") 'prev-buffer)

(define-key term-mode-map (kbd "C-x 0") 'delete-window)
(define-key term-mode-map (kbd "C-x 1") 'delete-other-windows)
(define-key term-mode-map (kbd "C-x 2") 'split-window-below)
(define-key term-mode-map (kbd "C-x 3") 'split-window-right)

(require 'simple)
; ess-smart-underscore ess-smart-equals  exwm xelb perspective fuzzy

(with-eval-after-load 'org-mode
  (define-key org-mode-map [(control tab)] 'next-buff))

(with-eval-after-load 'ivy
  (ido-mode nil)
  (ivy-mode nil)
  (setq ivy-do-completion-in-region nil)
  (ivy-mode t)
  (require 'ivy-utils))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "n") 'next-error-no-select)
  (define-key compilation-mode-map (kbd "p") 'previous-error-no-select))

(require 'ivy-rich-ext)

(require 'winstack-list)

(with-eval-after-load 'doom-themes
;; (require 'doom-themes)

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (setq doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme
  (load-theme 'doom-mhfc t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(with-eval-after-load 'powerline
  (require 'powerline-ext)
  (powerline-default-theme))

(with-eval-after-load 'counsel
  (require 'counsel-ag-ext)
  (global-set-key [(meta f)] 'counsel-ag-preselect))

(require 'xwidget-ext)

(with-eval-after-load 'magit-mode
  (define-key magit-mode-map [(control tab)] 'other-window))
(setq magit-completing-read-function 'ivy-completing-read)

(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "C-d") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map [(control return)] #'ivy-done)
  (define-key ivy-minibuffer-map [(meta t)] 'ivy-shell)
  (define-key ivy-minibuffer-map [(meta shift t)] 'ivy-new-shell)
  (define-key ivy-minibuffer-map [(meta p)] 'ivy-python)
  (define-key ivy-minibuffer-map sc-status-key 'ivy-magit-status)
  (define-key ivy-minibuffer-map (kbd "M-<up>") 'ivy-previous-history-element)
  (define-key ivy-minibuffer-map (kbd "M-<down>") 'ivy-next-history-element)
  (define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line-or-history-2)
  (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-occur)
  (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)

  (define-key ivy-minibuffer-map (kbd "C-l") 'my-toggle-truncate-lines)

  (global-set-key (kbd "M-c") 'ivy-resume)

  (with-eval-after-load 'swiper
    (setq swiper-include-line-number-in-search t)

    (define-key swiper-map (kbd "C-/") 'swiper-comment-or-uncomment-line)
    (define-key swiper-map (kbd "C-k") 'swiper-kill-line)
    (define-key swiper-map (kbd "M-f") 'swiper-convert-to-ag)
    (define-key swiper-map (kbd "<down>") 'ivy-next-line-and-call)
    (define-key swiper-map (kbd "<up>") 'ivy-previous-line-and-call)
    (define-key swiper-map (kbd "C-<up>") 'ivy-previous-line)
    (define-key swiper-map (kbd "C-<down>") 'ivy-next-line)
    (define-key swiper-map (kbd "M-r") 'ivy-rotate-preferred-builders)

    (define-key swiper-map (kbd "C-l") 'my-toggle-truncate-lines)
    (define-key swiper-map (kbd "M-C-p") 'swiper--goto-original-point))

  (with-eval-after-load 'counsel
    (require 'swiper-async)
    (global-set-key (kbd "C-s") 'swiper-async-search-forward)
    (global-set-key (kbd "C-r") 'swiper-async-search-backward)

    (define-key ivy-minibuffer-map (kbd "M-f") 'swiper-convert-to-ag)
    (define-key counsel-find-file-map (kbd "M-f") 'find-file-convert-to-ag)
    (define-key counsel-find-file-map (kbd "C-o") 'ivy-find-file-as-root)

    (define-key counsel-ag-map (kbd "<down>") 'ivy-next-line-and-call)
    (define-key counsel-ag-map (kbd "<up>") 'ivy-previous-line-and-call)
    (define-key counsel-ag-map (kbd "C-<up>") 'ivy-previous-line)
    (define-key counsel-ag-map (kbd "C-<down>") 'ivy-next-line)

    ;; (setq ido-file-extensions-order t)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-a") 'counsel-locate)
    (global-set-key (kbd "C-x p") 'counsel-git)
    )
  )

(with-eval-after-load "multiple-cursors-autoloads"
  (with-eval-after-load 'swiper
    (with-eval-after-load 'multiple-cursors-swiper
      (define-key swiper-map (kbd "C-g") 'mcs-minibuffer-keyboard-quit)
      (global-set-key (kbd "C-c C-a") 'mcs-swiper)
      (define-key swiper-map (kbd "C-SPC") 'mcs-toggle-cursor-at-point)
      (define-key swiper-map (kbd "C->") 'mcs-mark-next-like-this)
      (define-key swiper-map (kbd "C-<") 'mcs-mark-previous-like-this)))

  (require 'multiple-cursors-yank)
  (require 'multiple-cursors-sync-window)
  ;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "M-<f3>") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-l") 'mc/edit-ends-of-lines))

;; (require 'multiple-cursors)


;(global-set-key [(f2)] 'gud-break)

;; (require 'ahg) ; for mercurial source control, like magit
;; (with-eval-after-load 'ahg
;;   (global-set-key sc-status-key 'ahg-status))

(with-eval-after-load 'magit
  (global-set-key sc-status-key 'magit-status))

;(require 'ascope)
;(global-set-key [(meta f9)] 'ascope-find-this-text-stringy)
;(global-set-key [(control f3)] 'ascope-find-global-definition)
;(global-set-key [(control f4)] 'ascope-find-this-symbol)
; (global-set-key [(control f5)] 'ascope-find-functions-calling-this-function)
; (global-set-key [(control f6)] 'ascope-find-called-functions)
; (global-set-key [(control f7)] 'ascope-find-files-including-file)
; (global-set-key [(control f8)] 'ascope-find-all-symbol-assignments)
;(ascope-init "/home/ubuntu/sources/chromium/")

(with-eval-after-load 'websocket
  (require 'jss)) ; remote js debugger

;(require 'webkit)

(with-eval-after-load 'python-mode
  (require 'python-ext)

  (global-set-key [(meta p)] 'old-python)
  (global-set-key [(meta shift p)] 'new-python)
  (define-key shell-mode-map [(meta p)] 'old-python)
  (define-key shell-mode-map [(meta shift p)] 'new-python)

  (defalias 'py-complete-completion-at-point (lambda() nil))

  ;; (fringe-mode '(0 . nil))
  ;; (require 'anaconda-mode)
  (with-eval-after-load 'anaconda-mode
    (setq python-shell-interpreter "python3") ; needed for anaconda-mode
    (add-hook 'python-mode-hook 'anaconda-mode)
    ;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    (define-key anaconda-mode-map goto-def-key 'anaconda-mode-find-assignments)

    (with-eval-after-load 'company
      ;; (require 'company-anaconda)
      (with-eval-after-load 'company-anaconda
        (add-to-list 'company-backends 'company-anaconda)
        (setq py-complete-function 'company-complete)))))
;; (require 'python-mode)


;; (with-eval-after-load 'auto-complete
;;   (require 'autocomplete-ext))

(with-eval-after-load 'company
  (require 'company-ext))

(global-set-key complete-key 'completion-at-point)

(with-eval-after-load 'bash-completion
  (bash-completion-setup))
;; (require 'bash-completion)

(with-eval-after-load 'speedbar
  (with-eval-after-load 'sr-speedbar
    (require 'sr-speedbar-ext)
    ;; (global-set-key (kbd "C-e") 'sr-speedbar-toggle-keep-window)
    (add-hook 'speedbar-reconfigure-keymaps-hook
              '(lambda ()
                 (define-key speedbar-mode-map (kbd "<backspace>") 'speedbar-up-directory)
                 (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
                 (define-key speedbar-mode-map [left] 'speedbar-contract-line-or-go-up)
                 (define-key speedbar-mode-map [M-up] 'speedbar-restricted-prev)
                 (define-key speedbar-mode-map [M-down] 'speedbar-restricted-next)
                 (define-key speedbar-mode-map [up] 'speedbar-prev)
                 (define-key speedbar-mode-map [down] 'speedbar-next)
                 (define-key speedbar-mode-map (kbd "M-g") 'sr-speedbar-navigate)
                 (define-key speedbar-mode-map [(control meta p)] 'winstack-pop)
                 (define-key speedbar-mode-map [(control meta n)] 'winstack-next)
                 ))

    (global-set-key (kbd "C-e") 'sr-speedbar-toggle)))

(with-eval-after-load 'dumb-jump
  (with-eval-after-load 'ivy
    (require 'ivy-dumb-jump)
    (global-set-key goto-def-key 'dumb-jump-go)))

(which-function-mode 1)

;; (require 'ess)

(with-eval-after-load 'epc
  (require 'completion-epc) ; required for frida completion
  (setenv "PYTHONSTARTUP" (expand-file-name (concat CONFIGURATION-PATH "/py_epc_completion.py")))
  (defun shell-completion-prefix ()
    ;; Note that the input string does not include its terminal newline.
    (let (
          (proc (get-buffer-process (current-buffer)))
          )
      (when proc
        (widen)
        (let (
              (pmark (process-mark proc))
              )
          (when (>= (point) (marker-position pmark))
            (buffer-substring-no-properties pmark (point)))))))
  (epc-completion-add 'comint-mode 'comint-mode-hook 'shell-completion-prefix)

  (defun eshell-completion-prefix ()
    ;; Note that the input string does not include its terminal newline.
    (widen)
    (let (
          (pmark (eshell-beginning-of-input))
          )
      (when (>= (point) (marker-position pmark))
        (buffer-substring-no-properties pmark (point)))))
  (epc-completion-add 'eshell-mode 'eshell-mode-hook 'eshell-completion-prefix)

  (with-eval-after-load 'python-mode
    (require 'company-py-shell)))

;; (require 'persp-mode)
(with-eval-after-load 'persp-mode
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

  (define-key term-mode-map (kbd "M-1") 'perspsw1)
  (define-key term-mode-map (kbd "M-2") 'perspsw2)
  (define-key term-mode-map (kbd "M-3") 'perspsw3)
  (define-key term-mode-map (kbd "M-4") 'perspsw4)
  (define-key term-mode-map (kbd "M-5") 'perspsw5)
  (define-key term-mode-map (kbd "M-6") 'perspsw6)
  (define-key term-mode-map (kbd "M-7") 'perspsw7)
  (define-key term-mode-map (kbd "M-8") 'perspsw8)
  (define-key term-mode-map (kbd "M-9") 'perspsw9)
  (define-key term-mode-map (kbd "M-0") 'perspsw0)

  (require 'winstack) ; needs to sort out when having and not having persp-mode.
  (global-set-key pop-key 'winstack-pop)
  (global-set-key next-key 'winstack-next)

  (require 'persp-mode-ext))

(with-eval-after-load 'ediff
  (require 'ediff-ext))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (r-autoyas company-jedi company-quickhelp persp-mode debbugs ivy-rich pcre2el company-rtags company-math doom-themes demangle-mode daemons coverage charmap browse-at-remote bifocal powerline ag dumb-jump counsel sr-speedbar python python-mode swiper company-irony company-anaconda pungi bash-completion multiple-cursors magit-gerrit web-beautify json-mode websocket js-comint web-mode pyimport bind-key company-web company-irony-c-headers android-mode anaconda-mode company-shell company magit hydra ess))))

(let (
      (need-install nil)
      )
  (dolist (package package-selected-packages)
    (condition-case err
        (require package)
      (error (message "Error when loading %S: %S" package err) (setq need-install t))))
  (when need-install
    (package-refresh-contents)
    (package-install-selected-packages)))
