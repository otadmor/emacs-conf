(require 'multiple-cursors)
(require 'swiper)

(defun mc/find-cursor-at-point(p)
  (let ((result-cursor nil))
    (mc/for-each-fake-cursor
     (when (and (>= p (overlay-start cursor)) (<= p (overlay-end cursor)))
       (setq result-cursor cursor)))
    result-cursor))

(defun mc/toggle-cursor-at-point(p)
  (let (
        (fake-cursor (mc/find-cursor-at-point p))
        )
    (if (not (null fake-cursor))
        (mc/remove-fake-cursor fake-cursor)
      (mc/create-fake-cursor-at-point)))
  (mc/maybe-multiple-cursors-mode))

(defun mc/remove-cursor-at-point(p)
  (let (
        (fake-cursor (mc/find-cursor-at-point p))
        )
    (when (not (null fake-cursor))
        (mc/remove-fake-cursor fake-cursor)))
  (mc/maybe-multiple-cursors-mode))

(defun mcs-toggle-cursor-at-point () (interactive)
  (with-ivy-window
    (mc/toggle-cursor-at-point (point))
    (ivy--exhibit)))

(defun mcs-mark-next-like-this () (interactive)
  (with-ivy-window
    (mc/toggle-cursor-at-point (point))
    (ivy-next-line)
    (ivy--exhibit)))
(defun mcs-mark-previous-like-this () (interactive)
       (with-ivy-window
    (mc/toggle-cursor-at-point (point))
    (ivy-previous-line)
    (ivy--exhibit)))

(defun mcs-alt-done (&optional arg) (interactive "P")
  (with-ivy-window
    (unless (= (mc/num-cursors) 1)
      (mc/remove-cursor-at-point (point))))
  (ivy-alt-done arg))

(defun mcs-minibuffer-keyboard-quit () (interactive)
  (with-ivy-window
    (unless (= (mc/num-cursors) 1)
      (mc/remove-fake-cursors)))
  (minibuffer-keyboard-quit))


(defun mcs-done () (interactive)
  (if (= (with-ivy-window (mc/num-cursors)) 1)
      (ivy-done)
    (with-ivy-window
      (let (
            (p (overlay-start (car (last (mc/all-fake-cursors)))))
            )
        (mc/toggle-cursor-at-point p)
        (setq swiper--opoint p)))
    (minibuffer-keyboard-quit)))

(provide 'multiple-cursors-swiper)
