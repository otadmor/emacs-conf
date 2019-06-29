(setq helm-alive-p nil) ; fix sr-speedbur bug
(require 'sr-speedbar)
(sr-speedbar-refresh-turn-off)
(defun sr-speedbar-find-window-buffer()
  (setq sr-speedbar-window (get-buffer-window sr-speedbar-buffer-name))
  (if sr-speedbar-window
      (setq speedbar-buffer (window-buffer sr-speedbar-window))
    (setq speedbar-buffer nil)))


(setq sr-speedbar-buffer-name-orig sr-speedbar-buffer-name)
(defun persp-mode-speedbar-after-activate-hook(frame-or-window)
  (setq sr-speedbar-buffer-name (concat sr-speedbar-buffer-name-orig
                                        (if (null (get-current-persp))
                                            ""
                                          (safe-persp-name (get-current-persp)))))
  (setq sr-speedbar-window (get-buffer-window sr-speedbar-buffer-name)))
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
            (and speedbar-last-window (window-live-p speedbar-last-window))
          (select-window speedbar-last-window))
        (setq speedbar-last-window nil))
    (progn
      (sr-speedbar-open)
      (setq speedbar-last-window (frame-selected-window))
      (sr-speedbar-select-window))))

(provide 'sr-speedbar-ext)
