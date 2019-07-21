(require 'speedbar)
(require 'sr-speedbar)
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

(sr-speedbar-refresh-turn-off)

(setq sr-speedbar-buffer-name-orig sr-speedbar-buffer-name)

(push 'speedbar-shown-directories pmv/specific-vars)
(push 'speedbar-directory-contents-alist pmv/specific-vars)
(defun sr-speedbar-refresh-perspective ()
  (setq sr-speedbar-buffer-name (concat sr-speedbar-buffer-name-orig
                                        (if (null (get-current-persp))
                                            ""
                                          (safe-persp-name (get-current-persp)))))
  (setq sr-speedbar-window (get-buffer-window sr-speedbar-buffer-name))
  (if (not (sr-speedbar-window-exist-p sr-speedbar-window))
      (setq speedbar-buffer nil)
    (setq speedbar-buffer (window-buffer sr-speedbar-window))
    (sr-speedbar-navigate-dir (car (last speedbar-shown-directories)))))
(add-hook 'persp-variables-restored #'sr-speedbar-refresh-perspective)

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
              current-window))))
  (select-window sr-speedbar-window)
  sr-speedbar-window)

(defun sr-speedbar-navigate-dir (root-dir)
  (with-temp-buffer
    (setq default-directory root-dir)
    (speedbar-refresh t))
  root-dir)

(defun sr-speedbar-navigate() (interactive)
  (let (
        (root-dir (read-directory-name "Navigate Speedbar: "))
        )
    (sr-speedbar-navigate-dir root-dir)))

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

(defun speedbar-goto-parent ()
  (condition-case nil
      (speedbar-backward-list)
    (error nil))
  (speedbar-prev 1))

(defun speedbar-contract-line-or-go-up ()
  "Contract the line under the cursor."
  (interactive)
  (beginning-of-line)
  (condition-case nil
      (progn
	(re-search-forward ":\\s-*.-. "
			   (line-end-position))
	(forward-char -2)
	(speedbar-do-function-pointer))
    (error
     (speedbar-goto-parent)
     (speedbar-position-cursor-on-line))))

(defun buffer-close-in-dir (dirname)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (string-prefix-p dirname default-directory)
        (kill-buffer)))))

(defun speedbar-close-all-buffers-in-dir ()
  (interactive)
  (buffer-close-in-dir (speedbar-line-file)))


(with-eval-after-load 'persp-mode
  (add-to-list 'speedbar-frame-parameters (cons 'persp-ignore-wconf t))
  (add-hook 'persp-common-buffer-filter-functions
            #'(lambda (b) (string-prefix-p "*SPEEDBAR*" (buffer-name b)))))


(provide 'sr-speedbar-ext)
