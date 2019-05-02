(require 'dumb-jump)
(require 'ivy)
(require 'swiper-async)

(defun dumb-jump-ivy-minibuffer-keyboard-quit ()
  (interactive)
  (with-ivy-window
    (goto-char dumb-jump--opoint))
  (minibuffer-keyboard-quit))

(defun dumb-jump--goto-original-point()
  (interactive)
  (with-ivy-window
    (setq swiper--current-match-start dumb-jump--opoint)
    (setq swiper--current-line (string-to-number (format-mode-line "%l")))
    (goto-char dumb-jump--opoint)))

(setq dumb-jump-last-query nil)

(defalias 'dumb-jump-populate-regexes (lambda (look-for regexes variant)
  "Take list of REGEXES and populate the LOOK-FOR target and return that list."
  (setq dumb-jump-last-query look-for)
  (--map (dumb-jump-populate-regex it look-for variant) regexes)))

(defun dumb-jump-convert-to-ag ()
  (interactive)
  (ivy-quit-and-run
    (counsel-ag-preselect dumb-jump-last-query)))

(defvar dumb-jump-ivy-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down>") 'ivy-next-line-and-call)
    (define-key map (kbd "<up>") 'ivy-previous-line-and-call)
    (define-key map (kbd "C-<up>") 'ivy-previous-line)
    (define-key map (kbd "C-<down>") 'ivy-next-line)
    (define-key map (kbd "M-f") 'dumb-jump-convert-to-ag)
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
                               :history 'counsel-git-grep-history
                               ))
                )
            (unless res
              (goto-char dumb-jump--opoint))
            (unless (or res (string= ivy-text ""))
              (cl-pushnew ivy-text counsel-git-grep-history))
                                        ; (when swiper--reveal-mode
                                        ; (reveal-mode 1))
            ))))
(defalias 'dumb-jump-ivy-jump-to-selected 'dumb-jump-ivy-jump-to-selected-with-call)
(setq dumb-jump-selector 'ivy)

(provide 'ivy-dumb-jump)
