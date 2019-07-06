;;;  -*- lexical-binding: t -*-

(require 'powerline)
(defvar blacklist-powerline-mode-list '()
  "Hidden minor-modes list in powerline.")
(defun blacklist-powerline-mode-func (x)
  (let (
        (car-x (car x))
        (cadr-x (cadr x))
        )
    (when (and (symbolp car-x)
               (symbol-value car-x)
               (or (not (stringp cadr-x))
                   (null (member cadr-x blacklist-powerline-mode-list))))
      x)))

(defpowerline powerline-major-mode
  (when (null (member mode-name blacklist-powerline-mode-list))
    (propertize (format-mode-line mode-name)
                'mouse-face 'mode-line-highlight
                'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
                'local-map (let ((map (make-sparse-keymap)))
                             (define-key map [mode-line down-mouse-1]
                               `(menu-item ,(purecopy "Menu Bar") ignore
                                           :filter (lambda (_) (mouse-menu-major-mode-map))))
                             (define-key map [mode-line mouse-2] 'describe-mode)
                             (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                             map))))

(defpowerline powerline-minor-modes
  (mapconcat (lambda (mm)
               (propertize mm
                           'mouse-face 'mode-line-highlight
                           'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                           'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map
                                          [mode-line down-mouse-1]
                                          (powerline-mouse 'minor 'menu mm))
                                        (define-key map
                                          [mode-line mouse-2]
                                          (powerline-mouse 'minor 'help mm))
                                        (define-key map
                                          [mode-line down-mouse-3]
                                          (powerline-mouse 'minor 'menu mm))
                                        (define-key map
                                          [header-line down-mouse-3]
                                          (powerline-mouse 'minor 'menu mm))
                                        map)))
             (split-string (format-mode-line
                            (delq nil (mapcar 'blacklist-powerline-mode-func
                                              minor-mode-alist))))
             (propertize " " 'face face)))

(add-to-list 'blacklist-powerline-mode-list " Outl")
(add-to-list 'blacklist-powerline-mode-list "Emacs-Lisp")
(add-to-list 'blacklist-powerline-mode-list "Lisp Interaction")
(add-to-list 'blacklist-powerline-mode-list " AC")
(add-to-list 'blacklist-powerline-mode-list " ivy")
(add-to-list 'blacklist-powerline-mode-list "Py")
(add-to-list 'blacklist-powerline-mode-list "Messages")
(add-to-list 'blacklist-powerline-mode-list "company")

(provide 'powerline-ext)
