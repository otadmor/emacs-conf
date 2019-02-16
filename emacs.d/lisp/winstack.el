; split-window
(defvar winstack-stack '()
  "A Stack holding window states.
Use `winstack-push' and

`winstack-pop' to modify it.")
(defvar winstack-future-stack '()
  "A Stack holding window states.
Use `winstack-push' and
`winstack-pop' to modify it.")

(require 'persp-mode nil t)

(defun winstack-stack-push(item)
  (if persp-mode
      (progn
        (when (not (persp-parameter 'winstack-stack))
          (set-persp-parameter 'winstack-stack '()))
        (let (
              (ws (persp-parameter 'winstack-stack))
              )
          (push item ws)
          (set-persp-parameter 'winstack-stack ws)))
    (push item winstack-stack)))
(defun winstack-stack-first()
  (if persp-mode
      (progn
        (when (not (persp-parameter 'winstack-stack))
          (set-persp-parameter 'winstack-stack '()))
        (first (persp-parameter 'winstack-stack))
        )
  (first winstack-stack)))
(defun winstack-stack-pop()
  (if persp-mode
      (progn
        (when (not (persp-parameter 'winstack-stack))
          (set-persp-parameter 'winstack-stack '()))
        (let (
              (ws (persp-parameter 'winstack-stack))
              )
          (let (
                (item (pop ws))
                )
            (set-persp-parameter 'winstack-stack ws)
            item)))
  (pop winstack-stack)))
(defun winstack-stack-length()
  (if persp-mode
      (progn
        (when (not (persp-parameter 'winstack-stack))
          (set-persp-parameter 'winstack-stack '()))
        (length (persp-parameter 'winstack-stack))
        )
  (length winstack-stack)))


(defun winstack-future-stack-push(item)
  (if persp-mode
      (progn
        (when (not (persp-parameter 'winstack-future-stack))
          (set-persp-parameter 'winstack-future-stack '()))
        (let (
              (ws (persp-parameter 'winstack-future-stack))
              )
          (push item ws)
          (set-persp-parameter 'winstack-future-stack ws)))
    (push item winstack-future-stack)))
(defun winstack-future-stack-first()
  (if persp-mode
      (progn
        (when (not (persp-parameter 'winstack-future-stack))
          (set-persp-parameter 'winstack-future-stack '()))
        (first (persp-parameter 'winstack-future-stack))
        )
  (first winstack-future-stack)))
(defun winstack-future-stack-pop()
  (if persp-mode
      (progn
        (when (not (persp-parameter 'winstack-future-stack))
          (set-persp-parameter 'winstack-future-stack '()))
        (let (
              (ws (persp-parameter 'winstack-future-stack))
              )
          (let (
                (item (pop ws))
                )
            (set-persp-parameter 'winstack-future-stack ws)
            item)))
  (pop winstack-future-stack)))
(defun winstack-future-stack-length()
  (if persp-mode
      (progn
        (when (not (persp-parameter 'winstack-future-stack))
          (set-persp-parameter 'winstack-future-stack '()))
        (length (persp-parameter 'winstack-future-stack))
        )
  (length winstack-future-stack)))
(defun winstack-future-stack-clear()
  (if persp-mode
      (set-persp-parameter 'winstack-future-stack '())
  (length winstack-future-stack)))

(setq in-winstack nil)

(defun lock-winstack(orig-fun &rest args)
  (let (
        (res
         (when (not in-winstack)
           (setq in-winstack t)
           (apply orig-fun args)))
        )
    (setq in-winstack nil) res))

(defun winstack-push(&optional window important)
  (let (
        (buffer (window-buffer window))
        (point (window-point window))
        )
    (when (not (minibufferp buffer))
      (winstack-push-inner buffer point important))))
(advice-add 'winstack-push :around #'lock-winstack)

(defun same-buffer-point(o buffer point)
  (and
   (equal (second o) buffer)
   (equal (third o) point)))

(defun buffer-point-action(o buffer point important)
  (if (not (same-buffer-point o buffer point))
      0 ; put new
    (if important
    1 ; replace
    2 ; do nothing
    )))

; compare-window-configurations
(defun winstack-push-inner(buffer point &optional important)
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

(defun jump-to-buffer-point(buffer point)
  (if (buffer-live-p buffer)
      (let (
            (window (display-buffer buffer))
            )
        ;; (switch-to-buffer buffer)
        (set-window-point window point))))

(defun jump-to-item(item)
  (let (
        (buffer (second item))
        (point (third item))
        )
    (jump-to-buffer-point buffer point)))

(defun winstack-pop()
  "Pop the last window state off `winstack-stack' and apply it. takes from old-stack and put in new-stack"
  (interactive)
  (setq in-winstack t)
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
    (setq in-winstack t)
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
;(advice-add 'winstack-next :around #'lock-winstack)

(add-hook 'post-command-hook #'winstack-push)
;(add-hook 'pre-command-hook 'winstack-push)

(provide 'winstack)
