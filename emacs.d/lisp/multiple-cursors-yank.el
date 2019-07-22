(require 'multiple-cursors)

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;

(defun set-nth (index seq newval)
   "Set the INDEX th element of SEQ to NEWVAL.
 SEQ __is__ modified."
   (setcar (nthcdr index seq) newval))

(defun mcy/store-current-kill-ring-in-killed-rectangle()
  (let (
        (id mcy--create-order-id)
        )
    (while (<= (length killed-rectangle) id)
      (setq killed-rectangle (append killed-rectangle '(""))))
    (let (
          (killed-region (car kill-ring-yank-pointer))
          )
      (when killed-region
        (set-nth id killed-rectangle killed-region)))))

(defun mcy/load-current-kill-ring-from-killed-rectangle()
  (let (
        (id mcy--create-order-id)
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

(defun mcy--insert-killed-rectangle-to-kill-ring(trim-size)
  (when (< trim-size (length killed-rectangle)) (setcdr (nthcdr (1- trim-size) killed-rectangle) nil))
  (kill-new (join-killed-rectangle)))

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;

(setq mcy--create-order-id 0)
(setq mcy--ignore-first-store t)
(setq mcy--was-in-mc nil)
(setq mcy--last-cursors-amount 1)
(make-local-variable 'mcy--ignore-first-store)
(make-local-variable 'mcy--was-in-mc)
(push 'mcy--create-order-id mc/cursor-specific-vars)

(defun mcy/create-fake-cursor-at-point-hook(orig-fun &rest args)
  (let (
        (overlay (apply orig-fun args))
        )
    (when (null args)
      ;; remember the order of the cursor creation. this will be used
      ;; to know on which index to save the kill-ring of each cursor
      ;; on the killed-rectangle.
      (overlay-put overlay 'mcy--create-order-id (- (mc/num-cursors) 1)))
    overlay))
(advice-add 'mc/create-fake-cursor-at-point :around 'mcy/create-fake-cursor-at-point-hook)


(defun mcy/pre-command-hook()
  ;; load the curresponding kill-ring for each cursor
  ;; before each command. this loads the kill-ring
  ;; for both real and fake cursors.
  (setq mcy--was-in-mc t)
  (setq mcy--last-cursors-amount (mc/num-cursors))
  (mcy/load-current-kill-ring-from-killed-rectangle))

(defun mcy/post-command-hook()
  (if multiple-cursors-mode
      (if mcy--ignore-first-store
          (setq mcy--ignore-first-store nil)
        (mcy/store-current-kill-ring-in-killed-rectangle))
    ;; store the killed-rectangle to the real kill-ring
    ;; after each command execution when using
    ;; the multiple-cursors-mode.
    (when mcy--was-in-mc
      (setq mcy--ignore-first-store t)
      (mcy--insert-killed-rectangle-to-kill-ring mcy--last-cursors-amount)
      (setq mcy--was-in-mc nil))))

(defun mcy/mode-enabled()
  ;; the post command runs after entering the multiple-cursors-mode
  ;; this causes the join-killed-rectangle to be saved within itself
  ;; giving invalid multiple-cursors-yank yank function.
  ;; this variable disable this behaiviour on post command execution.
  (add-hook 'pre-command-hook 'mcy/pre-command-hook t t)
  (setq mcy--ignore-first-store t))

(defun mcy/mode-disabled()
  (remove-hook 'pre-command-hook 'mcy/pre-command-hook t))

(with-eval-after-load 'multiple-cursors
  (add-hook 'post-command-hook 'mcy/post-command-hook)
  (add-hook 'multiple-cursors-mode-enabled-hook 'mcy/mode-enabled)
  (add-hook 'multiple-cursors-mode-disabled-hook 'mcy/mode-disabled)

  ;; we dont want to change the killed-rectangle when
  ;; exiting multiple-cursor-mode. the killed-rectangle
  ;; is updated on each command. updating it when
  ;; exiting the multiple-cursors-mode will confuse
  ;; users when using two different buffers.
  (defalias 'mc--maybe-set-killed-rectangle (lambda() ))
  (define-key mc/keymap (kbd "<return>") nil))

(provide 'multiple-cursors-yank)
