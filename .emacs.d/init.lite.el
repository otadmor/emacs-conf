(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t); Enable stuff

(ido-mode t)

; I like a dark background
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode 'dark)
 '(package-selected-packages '(magit counsel ivy xclip)))

; I like bright text
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "white" :background "black"))))
 '(diff-added ((t (:background "#003000"))))
 '(diff-hunk-header ((t (:background "blue"))))
 '(diff-removed ((t (:background "#300000"))))
 '(font-lock-comment-face ((t (:foreground "green"))))
 '(font-lock-doc-face ((t (:foreground "green2"))))
 '(font-lock-function-name-face ((t (:foreground "gold"))))
 '(font-lock-keyword-face ((t (:foreground "red"))))
 '(font-lock-preprocessor-face ((t (:foreground "yellow"))))
 '(font-lock-string-face ((t (:foreground "cyan"))))
 '(font-lock-type-face ((t (:foreground "green3"))))
 '(font-lock-variable-name-face ((t (:foreground "aquamarine"))))
 '(font-lock-warning-face ((t (:foreground "#Ea0" :bold t))))
 '(isearch ((t (:background "cornflowerblue"))))
 '(region ((t (:background "#444444"))))
 '(show-paren-match ((t (:background "#444464"))))
 '(show-paren-mismatch ((t (:background "#600000")))))

; I like white cursor and mouse
(set-cursor-color "white")
(set-mouse-color "white")

; The tool-bar takes precious space. Remove it!
(tool-bar-mode -1)

; I prefer a custom greeting on the scratch buffer
(setq inhibit-splash-screen t)
(save-excursion
  (switch-to-buffer "*scratch*")
  (insert ";;")
  (newline)
  (insert ";; Welcome h4x0r !!!")
  (newline)
  (insert ";;")
  (newline))

; Blinking cursor is annoying
(blink-cursor-mode 0)

; Do not create back-up files
(setq make-backup-files nil)

; Clean-up trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)




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


; Global keys
(global-set-key [(meta g)] 'goto-line)
(global-set-key [(control x) (control c)] (defun dont-kill-emacs() (interactive) (message "Use C-x c to leave")))
(global-set-key [(control x) (c)] 'save-buffers-kill-emacs)
(global-set-key [(pause)] 'kill-this-buffer)
(global-set-key [(scroll-lock)] 'kill-this-buffer)
(global-set-key [(control tab)] 'other-window)
(global-set-key [(control z)] 'undo)
(global-set-key [f5] 'delete-other-windows)

(xclip-mode 1)
(xterm-mouse-mode)


(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(defun last-buffer() (interactive) (switch-to-buffer (other-buffer)))
(defun next-buff() (interactive) (other-window 1))
(defun prev-buffer() (interactive) (other-window -1))
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (when (active-minibuffer-window)
      (if (eq (active-minibuffer-window) (frame-selected-window))
          (select-window (get-mru-window))
          (select-window (active-minibuffer-window))
          )))


(global-set-key [(control tab)] 'next-buff)
(global-set-key (kbd "C-S-<iso-lefttab>") 'prev-buffer)

(global-set-key [(meta left)] 'backward-sexp)
(global-set-key [(meta right)] 'forward-sexp)

(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)

(global-set-key (kbd "M-d") 'switch-to-minibuffer)


(defun new-shell ()
  "create new shell"
  (interactive)
  (let (
        (b (generate-new-buffer "*shell*"))
        )
    (switch-to-buffer b)
    (shell b) b))

(global-set-key [(meta shift t)] 'new-shell)

(ivy-mode)
(setq swiper-include-line-number-in-search t)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)

(require 'comint)
(define-key comint-mode-map (kbd "M-r") 'counsel-shell-history)

(define-key comint-mode-map [(shift return)] 'newline)
(define-key comint-mode-map (kbd "C-c C-]") (lambda () (interactive) (insert (kbd "C-]")) (comint-send-input)))
(define-key comint-mode-map (kbd "C-p") 'comint-previous-matching-input-from-input)
(define-key comint-mode-map (kbd "C-n") 'comint-next-matching-input-from-input)



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
    (comment-or-uncomment-region beg end)))

(global-set-key (kbd "C-/") 'my-comment-or-uncomment-region)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'gdb-mode-hook 'ansi-color-for-comint-mode-on)


(require 'magit)
(define-key magit-mode-map [(control tab)] 'other-window)
(setq magit-completing-read-function 'ivy-completing-read)
(defvar sc-status-key (kbd "M-h"))
(defun ivy-magit-status ()
  (interactive)
  (if ivy--directory
      (ivy-quit-and-run
       (magit-status ivy--directory))
    (user-error
     "Not completing files currently")))
(define-key ivy-minibuffer-map sc-status-key 'ivy-magit-status)
(global-set-key sc-status-key 'magit-status)

(define-key ivy-minibuffer-map (kbd "C-d") #'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "M-<up>") 'ivy-previous-history-element)
(define-key ivy-minibuffer-map (kbd "M-<down>") 'ivy-next-history-element)
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)
(define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line-or-history)
(setq ivy-wrap t)

(setq CONFIGURATION-PATH (expand-file-name "~/.emacs.d/lisp"))
(setq load-path (cons CONFIGURATION-PATH load-path))

(require 'cl-lib)
(require 'cl)
(require 'too-long-lines-mode)
(too-long-lines-mode)
(put 'erase-buffer 'disabled nil)
