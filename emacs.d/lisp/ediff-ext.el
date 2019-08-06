;; -*- lexical-binding: t; -*-

(with-eval-after-load 'ediff-util
  (defun ediff-operate-on-windows-func (func &rest args)
    ;; make sure windows aren't dead
    (if (not (and (window-live-p ediff-window-A) (window-live-p ediff-window-B)))
        (ediff-recenter 'no-rehighlight))
    (if (not (and (ediff-buffer-live-p ediff-buffer-A)
                  (ediff-buffer-live-p ediff-buffer-B)
                  (or (not ediff-3way-job) (ediff-buffer-live-p ediff-buffer-C))
                  (or (not ediff-merge-with-ancestor-job)
                      (not ediff-show-ancestor)
                      (ediff-buffer-live-p ediff-ancestor-buffer))
                  ))
        (error ediff-KILLED-VITAL-BUFFER))

    (let* ((wind (selected-window))
           (wind-A ediff-window-A)
           (wind-B ediff-window-B)
           (wind-C ediff-window-C)
           (wind-Anc ediff-window-Ancestor)
           (three-way ediff-3way-job)
           (with-Ancestor (and ediff-merge-with-ancestor-job ediff-show-ancestor)))

      (select-window wind-A)
      (condition-case nil
          (apply func args)
        (error))
      (select-window wind-B)
      (condition-case nil
          (apply func args)
        (error))
      (if three-way
          (progn
            (select-window wind-C)
            (condition-case nil
                (apply func args)
              (error))))
      (when with-Ancestor
        (select-window wind-Anc)
        (condition-case nil
            (apply func args)
          (error)))
      (select-window wind)))

  (defun ediff-wrap-interactive (func)
    ;; (interactive "P")
    (defalias
      (make-symbol (concat "elisp---" (symbol-name func) "---wrapper"))
      (lambda (&rest args)
        (interactive (advice-eval-interactive-spec
                      (cadr (interactive-form func))))
        (ediff-barf-if-not-control-buffer)

        ;; make sure windows aren't dead
        (if (not (and (window-live-p ediff-window-A)
                      (window-live-p ediff-window-B)))
            (ediff-recenter 'no-rehighlight))
        (if (not (and (ediff-buffer-live-p ediff-buffer-A)
                      (ediff-buffer-live-p ediff-buffer-B)
                      (or (not ediff-3way-job)
                          (ediff-buffer-live-p ediff-buffer-C))
                      (or (not ediff-merge-with-ancestor-job)
                          (not ediff-show-ancestor)
                          (ediff-buffer-live-p
                           ediff-ancestor-buffer))
                      ))
            (error ediff-KILLED-VITAL-BUFFER))
        (apply 'ediff-operate-on-windows-func
               (cons func args)))
      (documentation func))))

(with-eval-after-load 'ediff
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-toggle-multiframe nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  (add-hook
   'ediff-keymap-setup-hook
   '(lambda ()
      (setq ediff-mode-map (make-keymap))
      (or (char-table-p (nth 1 ediff-mode-map))
          (error "The initialization of isearch-mode-map must be updated"))
      (define-key ediff-mode-map (kbd "<right>") (ediff-wrap-interactive #'right-char))
      (define-key ediff-mode-map (kbd "<left>") (ediff-wrap-interactive #'left-char))
      (define-key ediff-mode-map (kbd "<up>") (ediff-wrap-interactive #'previous-line))
      (define-key ediff-mode-map (kbd "<down>") (ediff-wrap-interactive #'next-line))
      (define-key ediff-mode-map (kbd "<next>") (ediff-wrap-interactive #'scroll-up-command))
      (define-key ediff-mode-map (kbd "<prior>") (ediff-wrap-interactive #'scroll-down-command))
      (define-key ediff-mode-map (kbd "<home>") (ediff-wrap-interactive #'move-beginning-of-line))
      (define-key ediff-mode-map (kbd "<end>") (ediff-wrap-interactive #'move-end-of-line))
      (define-key ediff-mode-map (kbd "C-<home>") (ediff-wrap-interactive #'beginning-of-buffer))
      (define-key ediff-mode-map (kbd "C-<end>") (ediff-wrap-interactive #'end-of-buffer))
      (define-key ediff-mode-map (kbd "M-<left>") (ediff-wrap-interactive #'backward-sexp))
      (define-key ediff-mode-map (kbd "M-<right>") (ediff-wrap-interactive #'forward-sexp))

      (define-key ediff-mode-map [backspace] (ediff-wrap-interactive #'backward-delete-char-untabify))
      (define-key ediff-mode-map "\177" (ediff-wrap-interactive #'delete-forward-char))
      (define-key ediff-mode-map (kbd "<deletechar>") (ediff-wrap-interactive #'delete-forward-char)) ; <delete>
      (define-key ediff-mode-map "\r" (ediff-wrap-interactive #'newline))
      (define-key ediff-mode-map "<return>" (ediff-wrap-interactive #'newline))
      (define-key ediff-mode-map [return] (ediff-wrap-interactive #'newline))
      (define-key ediff-mode-map (kbd "<tab>") (ediff-wrap-interactive #'indent-for-tab-command))

      (set-char-table-range (nth 1 ediff-mode-map) (cons #x100 (max-char))
                            (ediff-wrap-interactive #'self-insert-command))
      (let (
            (i 0)
            )
        (setq i ?\s)
        (while (< i 256)
          (define-key ediff-mode-map (vector i)
            (ediff-wrap-interactive #'self-insert-command))
          (setq i (1+ i))))

      (define-key ediff-mode-map (kbd "C-n") #'ediff-next-difference)
      (define-key ediff-mode-map (kbd "C-p") #'ediff-previous-difference)

      (define-key ediff-mode-map (kbd "C-?") #'ediff-toggle-help)

      )))

(provide 'ediff-ext)
