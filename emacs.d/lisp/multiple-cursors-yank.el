(require 'multiple-cursors)

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
      (setq killed-rectangle (append killed-rectangle '(""))))
    (let (
          (killed-region (car kill-ring-yank-pointer))
          )
      (when killed-region
        (set-nth id killed-rectangle killed-region)))))

(defun mc/load-current-kill-ring-from-killed-rectangle()
  (let (
        (id mc--create-order-id)
        )
    (let (
          (killed-region
           (if (and (< id (length killed-rectangle)) (>= id 0))
               (nth id killed-rectangle)
             (if (> (length killed-rectangle) 0)
                 (car killed-rectangle)
                 "")))
          )
      (when killed-region
        (kill-new killed-region)))))


(defun join-killed-rectangle()
  (string-join killed-rectangle "\n"))

(defun mc--insert-killed-rectangle-to-kill-ring()
  (kill-new (join-killed-rectangle)))

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;

(setq mc--create-order-id 0)
(setq mc--ignore-first-store t)
(setq mc--was-in-mc nil)
(make-local-variable 'mc--ignore-first-store)
(make-local-variable 'mc--was-in-mc)
(push 'mc--create-order-id mc/cursor-specific-vars)

(defun mc/create-fake-cursor-at-point-hook(orig-fun &rest args)
  (let (
        (overlay (apply orig-fun args))
        )
    (when (null args)
      ;; remember the order of the cursor creation. this will be used
      ;; to know on which index to save the kill-ring of each cursor
      ;; on the killed-rectangle.
      (overlay-put overlay 'mc--create-order-id (- (mc/num-cursors) 1)))
    overlay))
(advice-add 'mc/create-fake-cursor-at-point :around 'mc/create-fake-cursor-at-point-hook)


(defun mcy/pre-command-hook()
;  (message "PRE %S was in mc %S" mc--create-order-id mc--was-in-mc)
  ;; load the curresponding kill-ring for each cursor
  ;; before each command. this loads the kill-ring
  ;; for both real and fake cursors.
  (setq mc--was-in-mc t)
  (mc/load-current-kill-ring-from-killed-rectangle)
  )


(defun mcy/non-storing-post-command-hook()
;  (message "NO STORE POST %S was in mc %S" mc--create-order-id mc--was-in-mc)
  (add-hook 'post-command-hook 'mcy/storing-post-command-hook t t)
  (remove-hook 'post-command-hook 'mcy/non-storing-post-command-hook t)
  )

(defun mcy/storing-post-command-hook()
;  (message "STORE POST %S was in mc %S" mc--create-order-id mc--was-in-mc)
  ;; store the killed-rectangle to the real kill-ring
  ;; after each command execution when using
  ;; the multiple-cursors-mode.
  (mc/store-current-kill-ring-in-killed-rectangle))

(defun mcy/post-command-hook()
;  (message "POST %S was in mc %S" mc--create-order-id mc--was-in-mc)
  (when mc--was-in-mc
    (unless multiple-cursors-mode
;      (message "POST INSERT")
      (mc--insert-killed-rectangle-to-kill-ring)
;      (add-hook 'post-command-hook 'mcy/non-storing-post-command-hook t t)
;      (remove-hook 'post-command-hook 'mcy/storing-post-command-hook t)
      )
    (setq mc--was-in-mc nil)))
(add-hook 'post-command-hook 'mcy/post-command-hook)


(defun multiple-cursors-yank-mode-enabled()
  ;; the post command runs after entering the multiple-cursors-mode
  ;; this causes the join-killed-rectangle to be saved within itself
  ;; giving invalid multiple-cursors-yank yank function.
  ;; this variable disable this behaiviour on post command execution.
  (add-hook 'pre-command-hook 'mcy/pre-command-hook t t)
  (add-hook 'post-command-hook 'mcy/non-storing-post-command-hook t t)
 ; (setq mc--ignore-first-store t))
)
(add-hook 'multiple-cursors-mode-enabled-hook 'multiple-cursors-yank-mode-enabled)


(defun multiple-cursors-yank-mode-disabled()
;  (message "removed hooks")
  (remove-hook 'pre-command-hook 'mcy/pre-command-hook t)
  (remove-hook 'post-command-hook 'mcy/storing-post-command-hook t)
  (remove-hook 'post-command-hook 'mcy/non-storing-post-command-hook t)
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
