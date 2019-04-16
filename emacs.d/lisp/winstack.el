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

; (when (boundp 'pmv/cursor-specific-vars)
;     (push 'winstack-stack pmv/cursor-specific-vars)
;     (push 'winstack-future-stack pmv/cursor-specific-vars))


(defun winstack-to-persistent-parameter (stack)
  (if (null stack)
      nil
    (cl-mapcar
     (lambda (i)
       (list (first i) (second i) (third i) (winstack-point-from-marker (fourth i))))
     stack)))

(defun winstack-to-marker-stack (stack)
  (if (null stack)
      nil
    (let (
          (fn (buffer-file-name (current-buffer)))
          )
      (cl-mapcar
       (lambda (i)
         (if (not (string= (third i) fn))
             i
           (list (first i) (second i) (third i) (winstack-create-marker (fourth i)))))
       stack))))



(defun winstack-convert-to-marker ()
  (winstack-winstack-set (winstack-to-marker-stack (winstack-winstack-get)))
  (winstack-winstack-future-set (winstack-to-marker-stack (winstack-winstack-future-get))))

(defun winstack-point-from-marker (m)
  (if (markerp m)
      (marker-position m)
    m))

(defun winstack-create-marker (point)
  (set-marker (make-marker)
              (let ((mark-even-if-inactive t))
                point)))

(defun winstack-winstack-set (winstack-stack &optional window)
  (set-window-parameter window 'winstack-stack-marker winstack-stack))

(defun winstack-winstack-get (&optional window)
  (window-parameter window 'winstack-stack-marker))

(defun winstack-winstack-future-set (winstack-future-stack &optional window)
  (set-window-parameter window 'winstack-future-stack-marker winstack-future-stack))

(defun winstack-winstack-future-get (&optional window)
  (window-parameter window 'winstack-future-stack-marker))


(defun winstack-stack-push(item)
  (let (
        (winstack-stack (winstack-winstack-get))
        )
    (push item winstack-stack)
    (winstack-winstack-set winstack-stack)))

(defun winstack-stack-first()
  (first (winstack-winstack-get)))
(defun winstack-stack-pop()
  (let (
        (winstack-stack (winstack-winstack-get))
        )
    (let (
          (res (pop winstack-stack))
          )
      (winstack-winstack-set winstack-stack)
      res)))
(defun winstack-stack-length()
  (length (winstack-winstack-get)))


(defun winstack-future-stack-push(item)
  (let (
        (winstack-future-stack (winstack-winstack-future-get))
        )
    (push item winstack-future-stack)
    (winstack-winstack-future-set winstack-future-stack)))
(defun winstack-future-stack-first()
  (first (winstack-winstack-future-get)))
(defun winstack-future-stack-pop()
  (let (
        (winstack-future-stack (winstack-winstack-future-get))
        )
    (let (
          (res (pop winstack-future-stack))
          )
      (winstack-winstack-future-set winstack-future-stack)
      res)))
(defun winstack-future-stack-length()
  (length (winstack-winstack-future-get)))
(defun winstack-future-stack-clear()
  (winstack-winstack-future-set nil))

(setq in-winstack nil)

(defun winstack-push(&optional window important)
  (let (
        (buffer (window-buffer window))
        (point (window-point window))
        )
    (let (
          (bn (buffer-file-name buffer))
          (mark (winstack-create-marker point))
          )
      (when (and
             bn
             (not (minibufferp buffer))
             )
        (winstack-push-inner (if window window (selected-window)) bn mark important)))))

(defun same-buffer-point(o buffer point)
  (and
   (equal (third o) buffer)
   (equal (fourth o) point)))

(defun buffer-point-action(o buffer point important)
  (if (not (same-buffer-point o buffer point))
      0 ; put new
    (if important
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
          ;; (winstack-stack-push (list important window buffer point))
          ;;(message "winstack-push %S" item)
          (winstack-stack-push (list important nil buffer point))

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

(defun jump-to-buffer-point(window fn point)
  (let (
        (window (selected-window))
        )
    (let (
          (buffer (find-file fn))
          )
      (set-window-buffer window buffer)
      (goto-char point))))

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
(advice-add 'which-func-update-1 :around #'wrap-winstack-command-hook)

; (add-hook 'post-command-hook #'winstack-push)
; (add-hook 'pre-command-hook 'winstack-push)

(provide 'winstack)
