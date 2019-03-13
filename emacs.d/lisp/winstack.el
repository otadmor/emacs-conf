; split-window
(defvar winstack-stack '()
  "A Stack holding window states.
Use `winstack-push' and

`winstack-pop' to modify it.")
(defvar winstack-future-stack '()
  "A Stack holding window states.
Use `winstack-push' and
`winstack-pop' to modify it.")

(require 'persp-mode-variables nil t)

(when (boundp 'pmv/cursor-specific-vars)
    (push 'winstack-stack pmv/cursor-specific-vars)
    (push 'winstack-future-stack pmv/cursor-specific-vars))

(defun winstack-stack-push(item)
    (push item winstack-stack))
(defun winstack-stack-first()
  (first winstack-stack))
(defun winstack-stack-pop()
  (pop winstack-stack))
(defun winstack-stack-length()
  (length winstack-stack))


(defun winstack-future-stack-push(item)
    (push item winstack-future-stack))
(defun winstack-future-stack-first()
  (first winstack-future-stack))
(defun winstack-future-stack-pop()
  (pop winstack-future-stack))
(defun winstack-future-stack-length()
  (length winstack-future-stack))
(defun winstack-future-stack-clear()
  (length winstack-future-stack))

(setq in-winstack nil)

(defun winstack-push(&optional window important)
  (let (
        (buffer (window-buffer window))
        (point (window-point window))
        )
    (when (and
           (buffer-file-name buffer)
           (not (minibufferp buffer))
           )
      (winstack-push-inner (if window window (selected-window)) buffer point important))))

(defun same-buffer-point(o buffer point)
  (and
   (equal (third o) buffer)
   (equal (fourth o) point)))

(defun buffer-point-action(o buffer point important)
  (if (not (same-buffer-point o buffer point))
      0 ; put new
    (if (or important
            (not (window-live-p (second o))))
        1 ; replace
      2 ; do nothing
    )))

; compare-window-configurations
(defun winstack-push-inner(window buffer point &optional important)
    "Push the current window state onto `winstack-stack'."
    ;;(message "items before push important %S" my-important)
    ;;(print-elements-of-list winstack-future-stack)
    ;;(message ",")
    ;;(print-elements-of-list winstack-stack)
    ;; (message "winstack-push %S, %S" my-buffer my-point)
    (cl-loop
     repeat (winstack-future-stack-length) do
     (let (
           (future-item (winstack-future-stack-pop))
           )
       (let (
           (future-important (first future-item))
           )
         (when future-important (winstack-stack-push future-item)))))
    ;(winstack-future-stack-clear)
    (let (
          (action
           (if (> (winstack-stack-length) 0)
               (buffer-point-action (winstack-stack-first) buffer point important)
             0)) ; put new
          )
      (if (= action 2) ; do nothing
        (progn
          ;; (message "Current config already pushed %S" my-buffer)
          )
        (progn
          (when (= action 1) ; replace - pop existing items
              (winstack-stack-pop)
            )
          (let (
                (item '())
                )
            (push point item)
            (push buffer item)
            (push window item)
            (push important item)
            ;(message "winstack-push %S" item)
            (winstack-stack-push item)
            )
          ;; (message (concat "pushed " (number-to-string (length (window-list (selected-frame)))) " window state"))
          )
        )
      )
    ;;(message "items after push")
    ;;(print-elements-of-list winstack-future-stack)
    ;;(message ",")
    ;;(print-elements-of-list winstack-stack)
    )

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (message "%S" (car list))
    (setq list (cdr list))))

(defun jump-to-buffer-point(window buffer point)
  (if (buffer-live-p buffer)
      (let (
            (window (if (window-live-p window)
                        window
                      (let (
                            (w (selected-window))
                            )
                        (if (minibufferp (window-buffer w))
                            (get-mru-window)
                          w))))
            ;(window (display-buffer buffer))
            )
        (set-window-buffer window buffer)
        ;; (switch-to-buffer buffer)
        (set-window-point window point)
        (select-window window))))

(defun jump-to-item(item)
  (let (
        (window (second item))
        (buffer (third item))
        (point (fourth item))
        )
    (jump-to-buffer-point window buffer point)))

(defun winstack-pop()
  "Pop the last window state off `winstack-stack' and apply it. takes from old-stack and put in new-stack"
  (interactive)
    ;(message "items before pop")
    ;(print-elements-of-list winstack-stack)
    ;(message ",")
    ;(print-elements-of-list winstack-future-stack)
  (if (and (> (winstack-stack-length) 1))
      (let (
            (item (winstack-stack-pop))
            )
        ;(message "winstack-pop future %S" item)
        (winstack-future-stack-push item)
        (let (
              (current-item (winstack-stack-first))
              )
          ;(message "winstack-pop %S" current-item)
          (jump-to-item current-item)
          )
        )
    )
;    (message "items after pop")
;    (print-elements-of-list winstack-stack)
;    (message ",")
;    (print-elements-of-list winstack-future-stack)
)
;(advice-add 'winstack-pop :around #'lock-winstack)

(defun winstack-next()
    "Pop the last window state off `winstack-stack' and apply it. takes from old-stack and put in new-stack"
    (interactive)
    ;;(message "items before next")
    ;;(print-elements-of-list winstack-future-stack)
    ;;(message ",")
    ;; (print-elements-of-list winstack-future-stack)
    (if (> (winstack-future-stack-length) 0)
        (let (
              (current-item (winstack-future-stack-pop))
              )
          (winstack-stack-push current-item)
          ;(message "winstack-next %S" current-item)
          (jump-to-item current-item)
          )
      )
;    (message "items after next")
;    (print-elements-of-list winstack-future-stack)
;    (message ",")
;    (print-elements-of-list winstack-stack)
)

(defun wrap-winstack-hook(orig-fun &rest args)
  (if in-winstack
      (apply orig-fun args)
    (winstack-push)
    (setq in-winstack t)
    (let (
          (res (unwind-protect
                    (apply orig-fun args)
                  (setq in-winstack nil)))
          )
      (winstack-push)
      res)))

(defun wrap-winstack-command-hook(orig-fun &rest args)
  (interactive)
  (unless in-winstack
    (setq in-winstack t)
    (unwind-protect
        (apply orig-fun args)
      (setq in-winstack nil))))

(advice-add 'winstack-push :around #'wrap-winstack-command-hook)
(advice-add 'winstack-next :around #'wrap-winstack-command-hook)
(advice-add 'winstack-pop :around #'wrap-winstack-command-hook)

; (add-hook 'post-command-hook #'winstack-push)
; (add-hook 'pre-command-hook 'winstack-push)

(provide 'winstack)
