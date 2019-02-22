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

(defun mc--update-kill-ring(killed-region)
  (unless (eq killed-region (car kill-ring-yank-pointer))
    (push killed-region kill-ring-yank-pointer)
    (setq kill-ring kill-ring-yank-pointer)
    )
  )

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
        (mc--update-kill-ring killed-region)
        )
      )
    )
  )


(defun join-killed-rectangle()
  (string-join killed-rectangle "\n"))

(defun mc--insert-killed-rectangle-to-kill-ring()
  (mc--update-kill-ring (join-killed-rectangle))
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
      ;; remember the order of the cursor creation. this will be used
      ;; to know on which index to save the kill-ring of each cursor
      ;; on the killed-rectangle.
      (overlay-put overlay 'mc--create-order-id (- (mc/num-cursors) 1))
      )
    overlay
    )
  )
(advice-add 'mc/create-fake-cursor-at-point :around 'mc/create-fake-cursor-at-point-hook)


(defun multiple-cursors-pre-command-hook()
  (when multiple-cursors-mode
    ;; load the curresponding kill-ring for each cursor
    ;; before each command. this loads the kill-ring
    ;; for both real and fake cursors.
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

    ;; store the killed-rectangle to the real kill-ring
    ;; after each command execution when using
    ;; the multiple-cursors-mode.
    (when (= mc--create-order-id 0)
        (mc--insert-killed-rectangle-to-kill-ring)
      )
    )
  )
(add-hook 'post-command-hook 'multiple-cursors-post-command-hook)


(defun multiple-cursors-yank-mode-enabled()
  ;; the post command runs after entering the multiple-cursors-mode
  ;; this causes the join-killed-rectangle to be saved within itself
  ;; giving invalid multiple-cursors-yank yank function.
  ;; this variable disable this behaiviour on post command execution.
  (setq mc--ignore-first-store t)
  )
(add-hook 'multiple-cursors-mode-enabled-hook 'multiple-cursors-yank-mode-enabled)


(defun multiple-cursors-yank-mode-disabled()
  (mc--insert-killed-rectangle-to-kill-ring)
  )
(add-hook 'multiple-cursors-mode-disabled-hook 'multiple-cursors-yank-mode-disabled)


;; we dont want to change the killed-rectangle when
;; exiting multiple-cursor-mode. the killed-rectangle
;; is updated on each command. updating it when
;; exiting the multiple-cursors-mode will confuse
;; users when using two different buffers.
(defalias 'mc--maybe-set-killed-rectangle (lambda() ))

(define-key mc/keymap (kbd "<return>") nil)

(provide 'multiple-cursors-yank)

