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

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(setq-default frame-title-format '("%f [%m]"))

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
 '(default ((t (:family "Courier New" :height 100))))) ;;  :weight semibold
(setq-default line-spacing 1)

(setq CONFIGURATION-PATH (expand-file-name "~/.emacs.d/lisp"))
(setq load-path (cons (expand-file-name (concat CONFIGURATION-PATH "/jss-master")) load-path))
(setq load-path (cons CONFIGURATION-PATH load-path))

;; (set-cursor-color "white")
;; (set-mouse-color "white")

(setq helm-alive-p nil) ; fix sr-speedbur bug

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-style '(0 . 0))
(setq frame-resize-pixelwise nil)
(setq inhibit-splash-screen t)

;; Prevent the annoying beep on errors
(setq visible-bell t)

(blink-cursor-mode -1)
(delete-selection-mode 1)

(global-eldoc-mode -1)

; Do not create back-up files
(setq make-backup-files nil)

; Clean-up trailing whitespace on save
(add-hook 'before-save-hook (lambda () (when (not (eq 'diff-mode major-mode)) (delete-trailing-whitespace))))

; I hate tabs!
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(setq show-paren-style 'expression)
(setq show-paren-delay 0)
(show-paren-mode)

(setq select-enable-clipboard t)

(setq c-basic-offset 4)

(column-number-mode t)
(line-number-mode t)
;; (global-linum-mode 1)

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

(defun transient-xterm-paste (event)
  "Handle the start of a terminal paste operation."
  (interactive "e")
  (unless (eq (car-safe event) 'xterm-paste)
    (error "xterm-paste must be found to xterm-paste event"))
  (let ((pasted-text (nth 1 event)))
    (when (and transient-mark-mode mark-active)
      (delete-region (region-beginning) (region-end)))
    (if xterm-store-paste-on-kill-ring
        ;; Put the text onto the kill ring and then insert it into the
        ;; buffer.
        (let ((interprogram-paste-function (lambda () pasted-text)))
          (call-interactively 'yank))
      ;; Insert the text without putting it onto the kill ring.
      (push-mark)
      (insert-for-yank pasted-text))))

(defun setup-input-decode-map ()
  (with-eval-after-load 'xterm
    ;; Fix S-insert mapping
    ;; (define-key global-map [xterm-paste] 'transient-xterm-paste))
    (defalias 'xterm-paste 'transient-xterm-paste))
  (let ((map (if (boundp 'input-decode-map)
                 input-decode-map function-key-map)))
    ;; Fix CTRL + arrow keys inside screen/tmux
    (define-key map "\e[1;2A" [S-up])
    (define-key map "\e[1;2B" [S-down])
    (define-key map "\e[1;2C" [S-right])
    (define-key map "\e[1;2D" [S-left])
    (define-key map "\e[1;5A" [C-up])
    (define-key map "\e[1;5B" [C-down])
    (define-key map "\e[1;5C" [C-right])
    (define-key map "\e[1;5D" [C-left])

    ;; Fix ALT + arrow keys inside screen/tmux
    (define-key map "\e\e[1;3A" [M-up])
    (define-key map "\e\e[1;3B" [M-down])
    (define-key map "\e\e[1;3C" [M-right])
    (define-key map "\e\e[1;3D" [M-left])

    (define-key map "\e\e[1;7A" [C-M-up])
    (define-key map "\e\e[1;7B" [C-M-down])
    (define-key map "\e\e[1;7C" [C-M-right])
    (define-key map "\e\e[1;7D" [C-M-left])

    (define-key map "\e\e[1;4A" [S-M-up])
    (define-key map "\e\e[1;4B" [S-M-down])
    (define-key map "\e\e[1;4C" [S-M-right])
    (define-key map "\e\e[1;4D" [S-M-left])

    (define-key map "\e[1;6A" [S-C-up])
    (define-key map "\e[1;6B" [S-C-down])
    (define-key map "\e[1;6C" [S-C-right])
    (define-key map "\e[1;6D" [S-C-left])

    (define-key map "\e\e[1;8A" [S-C-M-up])
    (define-key map "\e\e[1;8B" [S-C-M-down])
    (define-key map "\e\e[1;8C" [S-C-M-right])
    (define-key map "\e\e[1;8D" [S-C-M-left])

    (define-key map "\e[13;2u" [(shift return)])
    (define-key map "\e[13;5u" [(control return)])
    (define-key map "\e[13;6u" [(control shift return)])
    (define-key map "\e[32;2u" [(shift space)])
    (define-key map "\e[32;5u" [C-SPC])
    (define-key map "\e[32;6u" [S-C-SPC])))
(add-hook 'tty-setup-hook #'setup-input-decode-map)

;(with-eval-after-load 'select
;  (require 'xclip2))
(defun getenv-display-hook (orig-fun VARIABLE &optional FRAME)
  (if (string= VARIABLE "DISPLAY")
      (replace-regexp-in-string "\n$" "" (shell-command-to-string "[[ \"\$TMUX\" != \"\" ]] && tmux switch-client -r > /dev/null 2>&1 && tmux switch-client -r > /dev/null 2>&1 && tmux show-env DISPLAY 2> /dev/null | grep -oP \"(?<==)(.*)\" || echo $DISPLAY"))
    (funcall orig-fun VARIABLE FRAME)))
(advice-add 'getenv :around #'getenv-display-hook)
(with-eval-after-load 'xclip
  (defun disable-xclip-on-tty ()
    (xclip-mode 0)
    (require 'osc52)
    (with-eval-after-load 'osc52
      ;; there might be a bug when calculating the actual length in the package osc52.
      ;; currently it takes the utf-8 string and multiplies it by 3.
      ;; the actual byte length of a utf-8 string is not easy to calculate (as each char have
      ;; different encoding bytes). anyway, i couldnt find the limit in tmux or windows terminal
      ;; and the number 2335289 is about the maximum i was able to copy, which is bigger than the
      ;; default 100000
      (setq osc52-max-sequence 2335289)
      (osc52-set-cut-function)))
  (add-hook 'tty-setup-hook #'disable-xclip-on-tty)
  (xclip-mode 1))
(xterm-mouse-mode)

(define-key function-key-map [select] [end])

(add-to-list 'auto-mode-alist '("\\.gdbinit\\'" . gdb-script-mode))
(add-to-list 'auto-mode-alist '("\\.mojo\\'" . idl-mode))
(add-to-list 'auto-mode-alist '("\\.mojom\\'" . idl-mode))

(require 'server-hook)

;; (find-file (expand-file-name "~/.emacs.d/server.log"))
;; (setq server-buffer (get-file-buffer "~/.emacs.d/server.log"))
;; (with-current-buffer server-buffer
;;   (set-buffer-auto-saved))
;; (setq server-log t)

(require 'scratch-util)

(require 'utils)

(global-set-key [(control x) (control c)] 'dont-kill-emacs)

(global-set-key [(control x) (c)] 'exit-emacs-or-close-frame)
(global-set-key [(control z)] 'undo)
(global-set-key [f9] 'compile)
;(global-set-key (kbd "C-l") 'my-toggle-truncate-lines)


(defvar next-key (kbd "C-M-n"))
(defvar pop-key (kbd "C-M-p"))
(defvar goto-def-key (kbd "C-M-g"))
(defvar find-ref-key (kbd "C-M-x"))
;; (defvar complete-key (kbd "C-SPC"))
(defvar sc-status-key (kbd "M-h"))

;; (global-set-key [f4] 'next-error)
;; (global-set-key [(shift f4)] 'previous-error)
(global-set-key [f1] 'manual-entry)
(global-set-key [f2] nil)
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
(global-set-key [(control l)] 'erase-buffer)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [f6] 'last-buffer)
(global-set-key [(control tab)] 'next-buff)
(global-set-key (kbd "C-S-<iso-lefttab>") 'prev-buffer)
(global-set-key [(pause)] 'kill-this-buffer)
(global-set-key (kbd "C-M-k") 'kill-this-buffer)
(global-set-key (kbd "C-x g") 'revert-buffer-quick)

(global-set-key [(meta left)] 'backward-sexp)
(global-set-key [(meta right)] 'forward-sexp)
;(global-set-key (kbd "C-h") 'backward-kill-word)

(global-set-key (kbd "C-x C-z") (lambda() (interactive) (message "Dont minimize")))
(global-set-key [(shift space)] (lambda (&optional n) (interactive "p") (self-insert-command n 32)))

;(global-set-key [(meta p)] 'run-python)
;(global-set-key [(meta shift p)] 'run-python)

;; ;; window resize (alt+shift)
;; (global-set-key [(meta shift left)] 'shrink-window-horizontally)
;; (global-set-key [(meta shift right)] 'enlarge-window-horizontally)
;; (global-set-key [(meta shift down)] 'enlarge-window)
;; (global-set-key [(meta shift up)] 'shrink-window)

(require 'shell)
;(define-key shell-mode-map [(meta p)] 'run-python)
;(define-key shell-mode-map [(meta shift p)] 'run-python)
(define-key shell-mode-map [(shift return)] 'newline)
(define-key shell-mode-map [(control return)] 'find-file-at-point)
(define-key shell-mode-map (kbd "C-<mouse-1>") (lambda () (interactive)))
(define-key shell-mode-map (kbd "C-<down-mouse-1>") (lambda () (interactive)
                                                      (call-interactively 'mouse-set-point)
                                                      (call-interactively 'find-file-at-point)))

(with-eval-after-load 'ffap
  (defun ffap-string-at-point--remove-asterisk-hook (orig-fun &rest args)
    (let ((res (apply orig-fun args)))
      (s-chop-suffix "*" (s-chop-suffix "$" res))))
  (defun ffap-read-file-or-url--open-direct-hook (orig-fun prompt guess)
    (if (file-exists-p guess)
        guess
      (funcall orig-fun prompt guess)))
  (advice-add 'ffap-string-at-point :around #'ffap-string-at-point--remove-asterisk-hook)
  (advice-add 'ffap-read-file-or-url :around #'ffap-read-file-or-url--open-direct-hook))

(require 'windmove)
(defun windmove-tmux-left (&optional arg) (interactive "P")
       (condition-case err
           (windmove-left arg)
         (user-error (if (null (getenv "TMUX"))
                         (message "%S" err)
                       (shell-command-to-string "if [ $(tmux display-message -p \"#{pane_at_left}\") -ne 1 ]; then tmux select-pane -L; fi")))))
(defun windmove-tmux-right (&optional arg) (interactive "P")
       (condition-case err
           (windmove-right arg)
         (user-error (if (null (getenv "TMUX"))
                         (message "%S" err)
                       (shell-command-to-string "if [ $(tmux display-message -p \"#{pane_at_right}\") -ne 1 ]; then tmux select-pane -R; fi")))))
(defun windmove-tmux-up (&optional arg) (interactive "P")
       (condition-case err
           (windmove-up arg)
         (user-error (if (null (getenv "TMUX"))
                         (message "%S" err)
                       (shell-command-to-string "if [ $(tmux display-message -p \"#{pane_at_top}\") -ne 1 ]; then tmux select-pane -U; fi")))))
(defun windmove-tmux-down (&optional arg) (interactive "P")
       (condition-case err
           (windmove-down arg)
         (user-error (if (null (getenv "TMUX"))
                         (message "%S" err)
                       (shell-command-to-string "if [ $(tmux display-message -p \"#{pane_at_bottom}\") -ne 1 ]; then tmux select-pane -D; fi")))))
(global-set-key (kbd "C-x <left>") 'windmove-tmux-left)
(global-set-key (kbd "C-x C-<left>") 'windmove-tmux-left)
(global-set-key (kbd "C-x <right>") 'windmove-tmux-right)
(global-set-key (kbd "C-x C-<right>") 'windmove-tmux-right)
(global-set-key (kbd "C-x <up>") 'windmove-tmux-up)
(global-set-key (kbd "C-x C-<up>") 'windmove-tmux-up)
(global-set-key (kbd "C-x <down>") 'windmove-tmux-down)
(global-set-key (kbd "C-x C-<down>") 'windmove-tmux-down)

(global-set-key (kbd "M-d") 'switch-to-minibuffer)

(global-set-key (kbd "C-x ,") 'rename-buffer)


;; (require 'redo+)
;; (global-set-key [(control z)] 'undo)
;; (global-set-key [(control y)] 'redo)
(setq-default explicit-shell-file-name "/bin/bash")
;;(setq-default shell-file-name "/bin/bash")

(require 'comint) ; to set comint-output-filter-functions
(define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
(define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map (kbd "C-p") 'comint-previous-matching-input-from-input)
(define-key comint-mode-map (kbd "C-n") 'comint-next-matching-input-from-input)
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

(require 'term)
(define-key term-mode-map [(control tab)] 'next-buff)
(define-key term-mode-map (kbd "C-S-<iso-lefttab>") 'prev-buffer)

(define-key term-mode-map (kbd "C-x 0") 'delete-window)
(define-key term-mode-map (kbd "C-x 1") 'delete-other-windows)
(define-key term-mode-map (kbd "C-x 2") 'split-window-below)
(define-key term-mode-map (kbd "C-x 3") 'split-window-right)

(require 'simple)
; ess-smart-underscore ess-smart-equals  exwm xelb perspective fuzzy

(require 'too-long-lines-mode)
(too-long-lines-mode)

(with-eval-after-load 'org
  (setq org-support-shift-select t)
  (setq org-replace-disputed-keys t)
  (setq org-disputed-keys
        '(
          ([(shift left)]          . [(meta -)])         ; change status (todo/closed/done)
          ([(shift right)]         . [(meta =)])         ;
          ([(shift up)]            . [(control meta -)]) ; change priority
          ([(shift down)]          . [(control meta =)]) ;
          ([(control shift right)] . [(meta +)])         ; status of group
          ([(control shift left)]  . [(meta _)])         ;
          ([(control shift up)]    . [(control meta +)]) ; change clock logs
          ([(control shift down)]  . [(control meta _)]) ;
          ))
  (define-key org-mode-map [(control tab)] 'next-buff)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (setq org-babel-python-command "python3")
  )

(with-eval-after-load 'ivy
  (setq enable-recursive-minibuffers t)
  (ido-mode nil)
  (ivy-mode nil)
  (setq ivy-do-completion-in-region nil)
  (ivy-mode t)
  (require 'ivy-utils))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "n") 'next-error-no-select)
  (define-key compilation-mode-map (kbd "p") 'previous-error-no-select))

;(require 'ivy-rich-ext)

(require 'winstack-list)

(with-eval-after-load 'doom-themes
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (setq doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme
  ;; (setq doom-mhfc-brighter-comments t)
  ;; (setq doom-mhfc-brighter-modeline t)
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
;;  (define-key ivy-minibuffer-map [(meta p)] 'ivy-python)
  (define-key ivy-minibuffer-map sc-status-key 'ivy-magit-status)
  (define-key ivy-minibuffer-map (kbd "M-<up>") 'ivy-previous-history-element)
  (define-key ivy-minibuffer-map (kbd "M-<down>") 'ivy-next-history-element)
  (define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line-or-history-2)
  (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-dired)
  (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)

  ;(define-key ivy-minibuffer-map (kbd "C-l") 'my-toggle-truncate-lines)

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
    (define-key swiper-map (kbd "C-s") 'ivy-next-line-or-history-and-call)
    (define-key swiper-map (kbd "C-r") 'ivy-previous-line-or-history-and-call)

    ;(define-key swiper-map (kbd "C-l") 'my-toggle-truncate-lines)
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
    (global-set-key (kbd "C-x a") 'counsel-locate)
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
  (global-set-key (kbd "C-\"") 'mc/mark-all-like-this)
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

;(with-eval-after-load 'websocket
;  (require 'jss)) ; remote js debugger

;(require 'webkit)


(with-eval-after-load 'deferred
  (defun deferred:promise (executor)
    (lexical-let (
                  (nd (deferred:new))
                  (executor executor)
                  )
      (deferred:next
        (lambda (_x)
          (condition-case err
              (funcall executor
                       (lambda (_xx) (deferred:callback-post nd _xx))
                       (lambda (_xx) (deferred:errorback-post nd _xx)))
            (error (deferred:errorback-post nd err)))
          nil))
      nd)))

(with-eval-after-load 'python-mode
  (require 'python-ext)

;;  (global-set-key [(meta p)] 'old-python)
;;  (global-set-key [(meta shift p)] 'new-python)
;;  (define-key shell-mode-map [(meta p)] 'old-python)
;;  (define-key shell-mode-map [(meta shift p)] 'new-python)

  (defalias 'py-complete-completion-at-point (lambda() nil))

  ;; (fringe-mode '(0 . nil))
  ;; (require 'anaconda-mode)
  )

(with-eval-after-load 'python
  (setq python-shell-interpreter "python3")) ; needed for anaconda-mode

(with-eval-after-load 'python-mode
  (add-hook 'python-mode-hook (lambda () (setq forward-sexp-function nil))))

(with-eval-after-load 'anaconda-mode
  (add-hook 'python-mode-hook 'anaconda-mode)
  (define-key anaconda-mode-map goto-def-key 'anaconda-mode-find-assignments)
  ;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

  (defun anaconda-completion-at-point()
    (unless (python-syntax-comment-or-string-p)
      (let* (
             (bounds (bounds-of-thing-at-point 'symbol))
             (start (or (car bounds) (point)))
             (stop (or (cdr bounds) (point)))
             )
        (list start
              stop
              (completion-table-dynamic
               (lambda (_)
                 (let ((completion-result nil))
                   (deferred:sync!
                     (deferred:$
                       (deferred:promise (lambda (callback errback) (anaconda-mode-call "complete" callback)))
                       (deferred:nextc it (lambda (reply) (setq completion-result reply)))))
                   (unless (null completion-result)
                     (--map (let* ((name (cdr (assoc 'name it)))
                                   (type (cdr (assoc 'type it)))
                                   (module-path (cdr (assoc 'module-path it)))
                                   (line (cdr (assoc 'line it)))
                                   (docstring (cdr (assoc 'docstring it)))
                                   (description (if (equal type "statement")
                                                    "statement"
                                                  (cdr (assoc 'description it)))))
                              (put-text-property 0 1 'description description name)
                              (put-text-property 0 1 'module-path module-path name)
                              (put-text-property 0 1 'line line name)
                              (put-text-property 0 1 'docstring docstring name)
                              name)
                            completion-result)))))))))
  (with-eval-after-load 'python
    (defalias 'python-completion-at-point 'anaconda-completion-at-point)))

;; (with-eval-after-load 'auto-complete
;;   (require 'autocomplete-ext))

(with-eval-after-load 'company
  (require 'company-ext))

;; (global-set-key complete-key 'completion-at-point)

(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (completion-at-point)
    (indent-for-tab-command)
    ))
;;(global-set-key (kbd "TAB") 'indent-or-complete)


;; (with-eval-after-load 'bash-completion
;;   (bash-completion-setup))
;; (require 'bash-completion)

(with-eval-after-load 'speedbar
  (with-eval-after-load 'sr-speedbar
    (require 'sr-speedbar-ext)
    ;; (global-set-key (kbd "C-e") 'sr-speedbar-toggle-keep-window)
    (defun setup-speedbar-keymap ()
      (define-key speedbar-mode-map (kbd "<backspace>") 'speedbar-up-directory)
      (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
      (define-key speedbar-mode-map [left] 'speedbar-contract-line-or-go-up)
      (define-key speedbar-mode-map [M-up] 'speedbar-restricted-prev)
      (define-key speedbar-mode-map [M-down] 'speedbar-restricted-next)
      (define-key speedbar-mode-map [up] 'speedbar-prev)
      (define-key speedbar-mode-map [down] 'speedbar-next)
      (define-key speedbar-mode-map (kbd "M-g") 'sr-speedbar-navigate)
      (define-key speedbar-mode-map [(control meta p)] 'winstack-pop)
      (define-key speedbar-mode-map [(control meta n)] 'winstack-next))
    (add-hook 'speedbar-reconfigure-keymaps-hook
              #'setup-speedbar-keymap)

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
  (global-set-key (kbd "M-1") (defun perspsw1() (interactive) (when persp-mode (persp-switch "1"))))
  (global-set-key (kbd "M-2") (defun perspsw2() (interactive) (when persp-mode (persp-switch "2"))))
  (global-set-key (kbd "M-3") (defun perspsw3() (interactive) (when persp-mode (persp-switch "3"))))
  (global-set-key (kbd "M-4") (defun perspsw4() (interactive) (when persp-mode (persp-switch "4"))))
  (global-set-key (kbd "M-5") (defun perspsw5() (interactive) (when persp-mode (persp-switch "5"))))
  (global-set-key (kbd "M-6") (defun perspsw6() (interactive) (when persp-mode (persp-switch "6"))))
  (global-set-key (kbd "M-7") (defun perspsw7() (interactive) (when persp-mode (persp-switch "7"))))
  (global-set-key (kbd "M-8") (defun perspsw8() (interactive) (when persp-mode (persp-switch "8"))))
  (global-set-key (kbd "M-9") (defun perspsw9() (interactive) (when persp-mode (persp-switch "9"))))
  (global-set-key (kbd "M-0") (defun perspsw0() (interactive) (when persp-mode (persp-switch "0"))))

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

(with-eval-after-load 'vdiff
  (require 'vdiff-ext))

(with-eval-after-load 'tern
  (add-hook 'js-mode-hook 'tern-mode)
  (add-hook 'js2-mode-hook 'tern-mode)
  (add-hook 'web-mode-hook 'tern-mode))

;; (with-eval-after-load 'which-key
;;   (which-key-mode))

(setq vterm-always-compile-module nil)
(require 'vterm)

;  ivy-rich
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-log-section-arguments '("--graph" "--decorate" "-n256"))
 '(package-selected-packages
   (quote
    (xclip ivy which-key tern vdiff company-jedi company-quickhelp persp-mode debbugs pcre2el company-rtags company-math doom-themes demangle-mode daemons coverage charmap browse-at-remote bifocal powerline ag dumb-jump counsel sr-speedbar python swiper company-irony pungi bash-completion multiple-cursors magit-gerrit web-beautify json-mode websocket js-comint web-mode pyimport bind-key company-web company-irony-c-headers android-mode anaconda-mode company-shell company magit hydra ess))))

(let (
      (need-install nil)
      )
  (dolist (package (reverse package-selected-packages))
    (condition-case err
        (require package)
      (error (message "Error when loading %S: %S" package err) (setq need-install t))))
  (when need-install
    (package-refresh-contents)
    (package-install-selected-packages)))
