; split-window
(defvar winstack-stack '()
  "A Stack holding window states.
Use `winstack-push' and
`winstack-pop' to modify it.")
(defvar winstack-future-stack '()
  "A Stack holding window states.
Use `winstack-push' and
`winstack-pop' to modify it.")

(defun winstack-push(&optional my-window important)
    (interactive)
    (let (
        (my-buffer (window-buffer my-window))
        (my-point (window-point my-window))
        )
            (winstack-mypush my-buffer my-point important)
    )
)

; compare-window-configurations
(defun winstack-mypush(my-buffer my-point &optional my-important)
    "Push the current window state onto `winstack-stack'."
    (interactive)
    ;(message "items before push important %S" my-important)
    ;(print-elements-of-list winstack-future-stack)
    ;(message ",")
    ;(print-elements-of-list winstack-stack)
    (message "winstack-push %S, %S" my-buffer my-point)
    (if (and
            (equal (second winstack-stack) my-buffer)
            (equal (third winstack-stack) my-point)
        )
        (progn
;            (message "Current config already pushed %S" my-buffer)
        )
        (progn
            (cl-loop repeat (/ (length winstack-future-stack) 3) do
                (let (
                    (orig-important (pop winstack-future-stack))
                    (orig-buffer (pop winstack-future-stack))
                    (orig-point (pop winstack-future-stack))
                    )
                        (if orig-important
                            (progn
                                (push orig-point winstack-stack)
                                (push orig-buffer winstack-stack)
                                (push orig-important winstack-stack)
                            )
                        )
                )
            )
            (setq winstack-future-stack '())
;            (push '('my-buffer . 'my-point) winstack-stack)
            (push my-point winstack-stack)
            (push my-buffer winstack-stack)
            (push my-important winstack-stack)
;            (message (concat "pushed " (number-to-string (length (window-list (selected-frame)))) " window state"))
        )
    )
    ;(message "items after push")
    ;(print-elements-of-list winstack-future-stack)
    ;(message ",")
    ;(print-elements-of-list winstack-stack)

)
(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (message "%S" (car list))
    (setq list (cdr list))))

(defun winstack-pop()
    "Pop the last window state off `winstack-stack' and apply it. takes from old-stack and put in new-stack"
    (interactive)
;    (if gdb-source-window
;        (winstack-push gdb-source-window t)
;    )
    ;(message "items before pop")
    ;(print-elements-of-list winstack-stack)
    ;(message ",")
    ;(print-elements-of-list winstack-future-stack)
    (if (second winstack-stack) ; (and
        ;
        ;     (fourth winstack-stack)
        ; )
        (let (
           (orig-important (pop winstack-stack))
            (orig-buffer (pop winstack-stack))
            (orig-point (pop winstack-stack))
            )
                (push orig-point winstack-future-stack)
                (push orig-buffer winstack-future-stack)
                (push orig-important winstack-future-stack)
                (let (
                    (cur-important (first winstack-stack))
                    (cur-buffer (second winstack-stack))
                    (cur-point (third winstack-stack))
                    )
                        (let (
                            (my-window (display-buffer cur-buffer))
                            )
                          (message "winstack-push %S, %S" cur-buffer cur-point)
                                (set-window-point my-window cur-point)
                        )
                )
        )
    )
;    (message "items after pop")
;    (print-elements-of-list winstack-stack)
;    (message ",")
;    (print-elements-of-list winstack-future-stack)
)

(defun winstack-next()
    "Pop the last window state off `winstack-stack' and apply it. takes from old-stack and put in new-stack"
    (interactive)
    ;(message "items before next")
    ;(print-elements-of-list winstack-future-stack)
    ;(message ",")
    ;(print-elements-of-list winstack-stack)
    (if (second winstack-future-stack)
        (let (
            (orig-important (pop winstack-future-stack))
            (orig-buffer (pop winstack-future-stack))
            (orig-point (pop winstack-future-stack))
            )
                (push orig-point winstack-stack)
                (push orig-buffer winstack-stack)
                (push orig-important winstack-stack)
                (let (
                    (my-window (display-buffer orig-buffer))
                    )
                          (message "winstack-push %S, %S" orig-buffer orig-point)
                        (set-window-point my-window orig-point)
                )
        )
    )
;    (message "items after next")
;    (print-elements-of-list winstack-future-stack)
;    (message ",")
;    (print-elements-of-list winstack-stack)
)

;(defun winstack-pop()
;    (interactive)
;;    (let (
;;        (res (winstack-mypop winstack-stack winstack-future-stack))
;;        )
;(winstack-mypop winstack-stack winstack-future-stack)
;            (message "received %S" res)
;            (setq winstack-stack (car res))
;            (setq winstack-future-stack (cdr res))
;;    )
;)
;(defun winstack-next()
;    (interactive)
;;    (let (
;;        (res (winstack-mypop winstack-future-stack winstack-stack))
;;        )
;(winstack-mypop winstack-future-stack winstack-stack)
;            (setq winstack-future-stack (car res))
;            (setq winstack-stack (cdr res))
;;    )
;)

(provide 'winstack)
