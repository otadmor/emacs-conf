(require 'multiple-cursors)

(defun set-multiple-cursors-mode() (interactive) (multiple-cursors-mode 1))
;(define-globalized-minor-mode global-multiple-cursors-mode multiple-cursors-mode 'set-multiple-cursors-mode)
;(global-multiple-cursors-mode)
;(add-hook 'after-init-hook 'global-multiple-cursors-mode)

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
                                        ; kill-ring-yank-pointer


(defun mc--maybe-set-killed-region ()
  "Add the latest kill-ring entry for each cursor to killed-rectangle.
So you can paste it in later with `yank-rectangle'."
  (let ((entries (let (mc/max-cursors) (mc--kill-ring-entries))))
      (setq killed-rectangle entries)))

(setq mc--fake-cursor-idx 0)
(defun set-nth (index seq newval)
   "Set the INDEX th element of SEQ to NEWVAL.
 SEQ __is__ modified."
   (setcar (nthcdr index seq) newval))

(defun mc/execute-command-for-all-fake-cursors-hook(orig-fun &rest args)
  (setq mc--fake-cursor-idx 0)
  (setq killed-rectangle '())
  ; (message "before all fakes %S" mc--fake-cursor-idx)
  (let (
        (res (apply orig-fun args))
        )
    ; (message "after all fakes %S" mc--fake-cursor-idx)
    (let (
          (id (- (mc/num-cursors) mc--fake-cursor-idx 1))
          )
      (when (= id 0) ; must be 0 after iterating over all the cursors
        (let (
              (killed-region (car kill-ring))
              )
          ; (message "update: killed rectangle id %S with %S" id killed-region)
          (set-nth id killed-rectangle killed-region)
          )
        )
      )
    (setq mc--fake-cursor-idx 0)
    res
    )
  )
(advice-add 'mc/execute-command-for-all-fake-cursors :around #'mc/execute-command-for-all-fake-cursors-hook)

(defun mc/execute-command-for-fake-cursor-hook(orig-fun &rest args)
  (let (
        (res (apply orig-fun args))
        )
    (setq mc--fake-cursor-idx (+ mc--fake-cursor-idx 1))
    res
    )
  )
(advice-add 'mc/execute-command-for-fake-cursor :around #'mc/execute-command-for-fake-cursor-hook)


(defun multiple-cursors-init-kill-buffer()
  (let (
        (killed-region (nth 0 killed-rectangle))
        )
    ; (message "init: killed region %S" killed-region)
    (when killed-region
      ; (message "init: old kill ring for %S is %S" 0 kill-ring)
      (push killed-region kill-ring)
      (setq kill-ring-yank-pointer kill-ring)
      ; (message "init: new kill ring for %S is %S" 0 kill-ring)
      )
    )
  )
(add-hook 'multiple-cursors-mode-enabled-hook 'multiple-cursors-init-kill-buffer)

(defun mc/reset-overlay-kill-ring (overlay id)
  (let (
        (killed-region
         (let (
               (killed-rectangle-size (length killed-rectangle))
               )
           (if (and (< id killed-rectangle-size) (>= id 0))
               (nth id killed-rectangle) "")
           )
         )
        )
    ;; (message "create: killed region for id %S < %S is %S" id killed-rectangle-size killed-region)
    (when killed-region
      (let (
            (overlay-kill-ring (list killed-region))
            (cursor-kill-ring (overlay-get overlay 'kill-ring))
            )
        ;; (message "create: old kill ring %S" cursor-kill-ring)
        (overlay-put overlay 'kill-ring overlay-kill-ring)
        (overlay-put overlay 'kill-ring-yank-pointer overlay-kill-ring)
        (overlay-put overlay 'mc--create-order-id id)
        ;; (message "create: new kill ring %S" (overlay-get overlay 'kill-ring))
        )
      )
    )
  )

(defun mc/create-fake-cursor-at-point-hook(orig-fun &rest args)
  (let (
        (id (- (mc/num-cursors) mc--fake-cursor-idx))
        (overlay (apply orig-fun args))
        )
    (if (car args)
        ;; (message "has id for %S" id)
        nil
      (if (= id 0)
          ; (message "not updating killed rectangle for id %S" id)
          nil
          (mc/reset-overlay-kill-ring overlay id)
          )
      )
    overlay
    )
  )
(advice-add 'mc/create-fake-cursor-at-point :around 'mc/create-fake-cursor-at-point-hook)

(defalias 'mc/for-each-fake-cursor-hook 'mc/for-each-cursor-ordered)
(setq mc--create-order-id 0)
(defun mc/execute-command-hook(orig-fun &rest args)
  (let (
        (res (apply orig-fun args))
        )
    (let (
          (cid mc--create-order-id)
          )
      (while (<= (length killed-rectangle) cid)
        (setq killed-rectangle (append killed-rectangle '("")))
        )
      (let (
            (killed-region (car kill-ring-yank-pointer))
            )
        ;; (message "update: killed rectangle id %S with %S" id killed-region)
        (set-nth cid killed-rectangle killed-region)
        )
      )
    res
    )
  )

(advice-add 'mc/execute-command :around 'mc/execute-command-hook)

(defun join-killed-rectangle()
  (string-join killed-rectangle "\n"))

(defun mc--insert-killed-rectangle-to-kill-ring() (kill-new (join-killed-rectangle)))
(defalias 'mc--maybe-set-killed-rectangle 'mc--insert-killed-rectangle-to-kill-ring)

(push 'mc--create-order-id mc/cursor-specific-vars)
(define-key mc/keymap (kbd "<return>") nil)

(defun multiple-cursors-focus-in-hook()
  (message "set killring focus")
  (when multiple-cursors-mode
    (message "set killring focus 0")
    (setq mc--fake-cursor-idx 0)
    (multiple-cursors-init-kill-buffer)
    (setq mc--fake-cursor-idx (+ mc--fake-cursor-idx 1))
    (mc/for-each-fake-cursor
     (message "set killring focus %S" mc--fake-cursor-idx)
     (mc/reset-overlay-kill-ring cursor mc--fake-cursor-idx)
     (setq mc--fake-cursor-idx (+ mc--fake-cursor-idx 1))
     )
    )
  )

(defun multiple-cursors-focus-out-hook()
  (message "insert to rectangle")
  (when multiple-cursors-mode
    (mc--insert-killed-rectangle-to-kill-ring)
    )
  )


(add-hook 'focus-in-hook 'multiple-cursors-focus-in-hook)
(add-hook 'focus-out-hook 'multiple-cursors-focus-out-hook)

(with-eval-after-load "persp-mode"
  (add-hook 'focus-in-hook 'multiple-cursors-focus-in-hook)
  (add-hook 'focus-out-hook 'multiple-cursors-focus-out-hook)
  )

(provide 'multiple-cursors-yank)

