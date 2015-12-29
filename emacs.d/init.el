; I like a dark background
(custom-set-variables
 '(frame-background-mode 'dark))

; I like bright text
(custom-set-faces
 '(default ((t (:foreground "white" :background "black" ))) t)
 '(diff-added ((t (:background "#003000"))))
 '(diff-hunk-header ((t (:background "blue"))))
 '(diff-removed ((t (:background "#300000"))))
 '(font-lock-comment-face ((t (:foreground "green"))))
 '(font-lock-doc-face ((t (:foreground "green2"))))
 '(font-lock-function-name-face ((t (:foreground "gold"))))
 '(font-lock-keyword-face ((t (:foreground "red" ))))
 '(font-lock-preprocessor-face ((t (:foreground "yellow"))))
 '(font-lock-string-face ((t (:foreground "cyan"))))
 '(font-lock-type-face ((t (:foreground "green3"))))
 '(font-lock-variable-name-face ((t (:foreground "aquamarine"))))
 '(font-lock-warning-face ((t (:foreground "#Ea0" :bold t))))
 '(isearch ((t (:background "cornflowerblue"))) t)
 '(show-paren-match ((t (:background "#444464"))) t)
 '(show-paren-mismatch ((t (:background "#600000"))) t)
 '(region ((t (:background "#444444"))) t)
 )

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

; Global keys

(global-set-key [(control x) (control c)] (defun dont-kill-emacs() (interactive) (message "Use C-x c to leave")))
(global-set-key [(control x) (c)] 'save-buffers-kill-emacs)
