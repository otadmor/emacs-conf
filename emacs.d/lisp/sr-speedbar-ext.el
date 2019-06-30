(setq helm-alive-p nil) ; fix sr-speedbur bug
(require 'sr-speedbar)
(sr-speedbar-refresh-turn-off)

(setq sr-speedbar-buffer-name-orig sr-speedbar-buffer-name)
(defun persp-mode-speedbar-after-activate-hook(frame-or-window)
  (setq sr-speedbar-buffer-name (concat sr-speedbar-buffer-name-orig
                                        (if (null (get-current-persp))
                                            ""
                                          (safe-persp-name (get-current-persp)))))
  (setq sr-speedbar-window (get-buffer-window sr-speedbar-buffer-name))
  (if (null sr-speedbar-window)
      (setq speedbar-buffer nil)
    (setq speedbar-buffer (window-buffer sr-speedbar-window))))
(add-hook 'persp-activated-functions #'persp-mode-speedbar-after-activate-hook)

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
(defun sr-speedbar-toggle-keep-window (orig-fun &rest args)
  (interactive)
  (let* (
         (before-open-selected-window (frame-selected-window))
         (speedbar-shown-before (sr-speedbar-window-exist-p sr-speedbar-window))
         (res (apply orig-fun args))
         (speedbar-shown-after (sr-speedbar-window-exist-p sr-speedbar-window))
        )
    (when (and speedbar-shown-before
               (not speedbar-shown-after))
      (when (and speedbar-last-window
                 (window-live-p speedbar-last-window))
        (select-window speedbar-last-window))
      (setq speedbar-last-window nil))
    (when (and (not speedbar-shown-before)
               speedbar-shown-after)
      (setq speedbar-last-window before-open-selected-window)
      (sr-speedbar-select-window))
    res))
(advice-add 'sr-speedbar-toggle :around #'sr-speedbar-toggle-keep-window)

(provide 'sr-speedbar-ext)
