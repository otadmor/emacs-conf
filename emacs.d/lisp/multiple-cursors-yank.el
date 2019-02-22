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
      (when killed-region
        (let (
              (new-kill-ring (list killed-region))
              )
          (push killed-region kill-ring-yank-pointer)
          (setq kill-ring kill-ring-yank-pointer)
          )
        )
      )
    )
  )


(defun join-killed-rectangle()
  (string-join killed-rectangle "\n"))
(defun mc--insert-killed-rectangle-to-kill-ring()
  (push (join-killed-rectangle) kill-ring-yank-pointer)
  (setq kill-ring kill-ring-yank-pointer)
  )

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;

(setq mc--create-order-id 0)
(setq mc--ignore-first-store t)
(make-local-variable 'mc--ignore-first-store)
(push 'mc--create-order-id mc/cursor-specific-vars)

(defun mc/create-fake-cursor-at-point-hook(orig-fun &rest args)
  (let (
        (overlay (apply orig-fun args))
        )
    (when (null args)
      (overlay-put overlay 'mc--create-order-id (- (mc/num-cursors) 1))
      )
    overlay
    )
  )
(advice-add 'mc/create-fake-cursor-at-point :around 'mc/create-fake-cursor-at-point-hook)


(defun multiple-cursors-pre-command-hook()
  (when multiple-cursors-mode
    (mc/load-current-kill-ring-from-killed-rectangle)
    )
  )
(add-hook 'pre-command-hook 'multiple-cursors-pre-command-hook)


(defun multiple-cursors-post-command-hook()
  (when multiple-cursors-mode
    (if mc--ignore-first-store
        (setq mc--ignore-first-store nil)
      (mc/store-current-kill-ring-in-killed-rectangle)
      )
    (when (= mc--create-order-id 0)
        (mc--insert-killed-rectangle-to-kill-ring)
      )
    )
  )
(add-hook 'post-command-hook 'multiple-cursors-post-command-hook)


(defun multiple-cursors-yank-mode-enabled()
  (setq mc--ignore-first-store t)
  )
(add-hook 'multiple-cursors-mode-enabled-hook 'multiple-cursors-yank-mode-enabled)


(defun multiple-cursors-yank-mode-disabled()
  (mc--insert-killed-rectangle-to-kill-ring)
  )
(add-hook 'multiple-cursors-mode-disabled-hook 'multiple-cursors-yank-mode-disabled)

(defalias 'mc--maybe-set-killed-rectangle (lambda() ))

(define-key mc/keymap (kbd "<return>") nil)

(provide 'multiple-cursors-yank)

