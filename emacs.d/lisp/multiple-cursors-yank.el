(require 'multiple-cursors)

; (defun swiper-mc ()
;   (interactive)
;   (unless (require 'multiple-cursors nil t)
;     (error "multiple-cursors isn't installed"))
;   (let ((cands (nreverse ivy--old-cands)))
;     (unless (string= ivy-text "")
;       (ivy-set-action
;        (lambda (_)
;          (let (cand)
;            (while (setq cand (pop cands))
;              (swiper--action cand)
;              (when cands
;                (mc/create-fake-cursor-at-point))))
;          (mc/maybe-multiple-cursors-mode)))
;       (setq ivy-exit 'done)
;       (exit-minibuffer))))


;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;

(defun set-nth (index seq newval)
   "Set the INDEX th element of SEQ to NEWVAL.
 SEQ __is__ modified."
   (setcar (nthcdr index seq) newval))

(defun mc/store-current-kill-ring-in-killed-rectangle()
  (let (
        (id mc--create-order-id)
        )
    (while (<= (length killed-rectangle) id)
      (setq killed-rectangle (append killed-rectangle '("")))
      )
    (let (
          (killed-region (car kill-ring-yank-pointer))
          )
      ;; (message "update: killed rectangle id %S with %S" id killed-region)
      (when killed-region
        (set-nth id killed-rectangle killed-region)
        )
      )
    )
  )

(defun mc/load-current-kill-ring-from-killed-rectangle()
  (let (
        (id mc--create-order-id)
        )
    (let (
          (killed-region
           (if (and (< id (length killed-rectangle)) (>= id 0))
               (nth id killed-rectangle) "")
           )
          )
      ;; (message "create: killed region for id %S < %S is %S" id killed-rectangle-size killed-region)
      (when killed-region
        (let (
              (new-kill-ring (list killed-region))
              )
          ;; (message "create: old kill ring %S" cursor-kill-ring)
          (push killed-region kill-ring-yank-pointer)
          (setq kill-ring kill-ring-yank-pointer)
          ;; (message "create: new kill ring %S" (overlay-get overlay 'kill-ring))
          )
        )
      )
    )
  )

(defun join-killed-rectangle()
  (string-join killed-rectangle "\n"))
(defun mc--insert-killed-rectangle-to-kill-ring() (kill-new (join-killed-rectangle)))

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;


(defun mc/execute-command-for-all-fake-cursors-hook(orig-fun &rest args)
  ; (message "before all fakes")
  (let (
        (res (apply orig-fun args))
        )
    ;; (message "after all fakes")
    (mc/store-current-kill-ring-in-killed-rectangle)
    res
    )
  )
(advice-add 'mc/execute-command-for-all-fake-cursors :around #'mc/execute-command-for-all-fake-cursors-hook)


(defun multiple-cursors-init-kill-buffer()
  (mc/load-current-kill-ring-from-killed-rectangle)
  )
(add-hook 'multiple-cursors-mode-enabled-hook 'multiple-cursors-init-kill-buffer)

(setq mc--create-order-id 0)
(push 'mc--create-order-id mc/cursor-specific-vars)

(defun mc/create-fake-cursor-at-point-hook(orig-fun &rest args)
  (let (
        (id (mc/num-cursors))
        (overlay (apply orig-fun args))
        )
    (unless (car args)
      (overlay-put overlay 'mc--create-order-id id)
      )
    overlay
    )
  )
(advice-add 'mc/create-fake-cursor-at-point :around 'mc/create-fake-cursor-at-point-hook)

(defun mc/execute-command-hook(orig-fun &rest args)
  (mc/load-current-kill-ring-from-killed-rectangle)
  (let (
        (res (apply orig-fun args))
        )
    (mc/store-current-kill-ring-in-killed-rectangle)
    res
    )
  )
(advice-add 'mc/execute-command :around 'mc/execute-command-hook)

(defalias 'mc--maybe-set-killed-rectangle 'mc--insert-killed-rectangle-to-kill-ring)

(define-key mc/keymap (kbd "<return>") nil)

(provide 'multiple-cursors-yank)

