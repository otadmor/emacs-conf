;; -*- lexical-binding: t; -*-
;; (require 'multiple-cursors)
;; (require 'swiper)
;; (require 'cl-lib)

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
    (mc/restore-all-cursors-states mc-swiper--backedup-cursors)
    (setq mc-swiper--backedup-cursors nil))
  (minibuffer-keyboard-quit))


(defun swiper--fill-candidate-properties (str swiper--format-spec line-no use-marker &optional begin end line-begin line-end)
  (setq str (ivy-cleanup-string str))
  (let ((line-number-str (format swiper--format-spec line-no)))
    (if swiper-include-line-number-in-search
        (setq str (concat line-number-str str))
      (put-text-property 0 1 'display line-number-str str))
    (put-text-property
       0 1 'swiper-line-number line-number-str str))
  (put-text-property
   0 1 'swiper-no-line-number line-no str)
  (put-text-property
   0 1 'region-data (if use-marker
                        (list
                         (set-marker (make-marker)
                                     (let ((mark-even-if-inactive t))
                                       end))
                         (set-marker (make-marker)
                                     (let ((mark-even-if-inactive t))
                                       begin)))
                      nil) str)
  (put-text-property
   0 1 'line-region-data (if use-marker
                             (list
                              (set-marker (make-marker)
                                          (let ((mark-even-if-inactive t))
                                            line-end))
                              (set-marker (make-marker)
                                          (let ((mark-even-if-inactive t))
                                            line-begin)))
                      nil) str)
  str)

(cl-defun swiper--mcs-candidates (&optional numbers-width &key advancer1 initiater1 use-marker use-format-mode-line include-empty-last-line)
  "Return a list of this buffer lines.
NUMBERS-WIDTH, when specified, is used for width spec of line
numbers; replaces calculating the width from buffer line count."
  (let* ((inhibit-field-text-motion t)
         (n-lines (count-lines (point-min) (point-max))))
    (if (and visual-line-mode
             ;; super-slow otherwise
             (< (buffer-size) 20000)
             (< n-lines 400))
        (progn
          (when (eq major-mode 'org-mode)
            (require 'outline)
            (if (fboundp 'outline-show-all)
                (outline-show-all)
              (with-no-warnings
                (show-all))))
          (setq swiper-use-visual-line t))
      (setq swiper-use-visual-line nil))
    (unless (zerop n-lines)
      (setq swiper--width (or numbers-width
                              (1+ (floor (log n-lines 10)))))
      (setq swiper--format-spec
            (format "%%-%dd " swiper--width))
      (let ((line-number 0)
            (advancer (if advancer1 advancer1
                        (if swiper-use-visual-line
                            (lambda (arg) (line-move arg t))
                          #'forward-line)))
            (initiater (if initiater1 initiater1
                         (lambda () (goto-char (point-min)))))
            candidates)
        (mc/save-excursion
         (mc/save-window-scroll
          (save-excursion
            (funcall initiater)
            (swiper-font-lock-ensure)
            (while (< (point) (point-max))
              (push (swiper--fill-candidate-properties
                     (save-excursion (beginning-of-line) (swiper--line))
                     swiper--format-spec
                     (if use-format-mode-line
                         (setq line-number (string-to-number (format-mode-line "%l")))
                       (cl-incf line-number)
                       )
                     use-marker
                     (mark) (point))
                    candidates)
              (funcall advancer 1))
            (when (and include-empty-last-line
                       (= (line-beginning-position) (line-end-position)))
              (push (swiper--fill-candidate-properties
                     (save-excursion (beginning-of-line) (swiper--line))
                     swiper--format-spec
                     (if use-format-mode-line
                         (setq line-number (string-to-number (format-mode-line "%l")))
                       (cl-incf line-number)
                       )
                     use-marker
                     (mark) (point))
                    candidates)))))
        (nreverse candidates)))))

(defun mcs-get-end-points()
  (let (
        (cursors '())
        )
    (mc/for-each-cursor-ordered
     (push (list
            (copy-marker (overlay-get cursor 'mark))
            (copy-marker (overlay-get cursor 'point))
            ) cursors))
    (reverse cursors)))


(defun swiper--mcs-update-input-ivy ()
  "Called when `ivy' input is updated."
  (with-ivy-window
    (swiper--cleanup)
    (when (> (length (ivy-state-current ivy-last)) 0)
      (let* ((regexp-or-regexps (funcall ivy--regex-function ivy-text))
             (regexps
              (if (listp regexp-or-regexps)
                  (mapcar #'car (cl-remove-if-not #'cdr regexp-or-regexps))
                (list regexp-or-regexps))))
        (dolist (re regexps)
          (let* ((re (replace-regexp-in-string
                      "    " "\t"
                      re))
                 (str (get-text-property 0 'swiper-line-number (ivy-state-current ivy-last)))
                 (num (if (string-match "^[0-9]+" str)
                          (string-to-number (match-string 0 str))
                        0)))
            (unless (memq this-command '(ivy-yank-word
                                         ivy-yank-symbol
                                         ivy-yank-char
                                         scroll-other-window
                                         swiper-async))
              (when (cl-plusp num)
                (unless (if swiper--current-line
                            (eq swiper--current-line num)
                          (eq (line-number-at-pos) num))
                  (goto-char swiper--point-min)
                  (if swiper-use-visual-line
                      (line-move (1- num))
                    (forward-line (1- num))))

                (when (> (mc/num-cursors) 1)
                    (deactivate-mark))
                (let (
                      (region-data (get-text-property 0 'region-data (ivy-state-current ivy-last)))
                      )
                  (if (not (null region-data))
                      (progn
                        (setq swiper--current-match-start (car region-data))
                        (setq swiper--current-line num)

                        (goto-char (car region-data))
                        )

                    (if (and (equal ivy-text "")
                             (>= swiper--opoint (line-beginning-position))
                             (<= swiper--opoint (line-end-position)))

                        (goto-char swiper--opoint)

                      (if (eq swiper--current-line num)
                          (when swiper--current-match-start
                            (goto-char swiper--current-match-start))
                        (setq swiper--current-line num))


                      (when (re-search-forward re (line-end-position) t)
                        (setq swiper--current-match-start (match-beginning 0)))


                    )))
                (isearch-range-invisible (line-beginning-position)
                                         (line-end-position))
                (when (and (display-graphic-p)
                           (or
                            (< (point) (window-start))
                            (> (point) (window-end (ivy-state-window ivy-last) t))))
                  (recenter))
                (setq swiper--current-window-start (window-start))))
            (swiper--add-overlays
             re
             (max
              (if (display-graphic-p)
                  (window-start)
                (line-beginning-position (- (window-height))))
              swiper--point-min)
             (min
              (if (display-graphic-p)
                  (window-end (selected-window) t)
                (line-end-position (window-height)))
              swiper--point-max))))))))

(setq mc-swiper--backedup-cursors nil)
(make-local-variable 'mc-swiper--backedup-cursors)

(defun mc/get-cursor-state(cursor)
  (mc/restore-state-from-overlay cursor)
  (let (
        (backup-overlay (mc/store-current-state-in-overlay
                         (make-overlay (point) (point) nil nil t)))
        )
    (overlay-put backup-overlay 'type 'backup-type)
    backup-overlay))

(defun mc/store-all-cursors-states()
  (let (
        (backedup-cursors '())
        )
    (mc/save-excursion
     (mc/save-window-scroll
      (when (> (mc/num-cursors) 1)
        (mc/for-each-cursor-ordered
         (save-excursion (push (mc/get-cursor-state cursor) backedup-cursors)))
        (mc/create-fake-cursor-at-point))))
    (reverse backedup-cursors)))

(defun poplast (l)
  (let (
        (lni (last l))
        )
      (nbutlast l)
      (car lni)))

(defun mc/restore-all-cursors-states(backedup-cursors)
  (mc/remove-fake-cursors)
  (cl-loop repeat (length backedup-cursors) do
           (let (
                 (backup-overlay (poplast backedup-cursors))
                 )
             (overlay-put backup-overlay 'type 'fake-cursor)
             (mc/pop-state-from-overlay backup-overlay)
             (mc/create-fake-cursor-at-point)))
  (mc/remove-cursor-at-point (point))
  (mc/maybe-multiple-cursors-mode))

(defun mcs-swiper (&optional initial-input)
  "`isearch' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (let (
        (cursors (mcs-get-end-points))
        (i 0)
        )
    (let (
          (mc-advancer (lambda (&optional n)
                         (if (>= i (length cursors))
                             (progn
                               (goto-char (point-max))
                               (deactivate-mark)
                               )
                           (let (
                                 (cursor (nth i cursors))
                                 )
                             (deactivate-mark)
                             (goto-char (cadr cursor))
                             (set-marker (mark-marker) (car cursor))
                             (activate-mark)
                             )
                           (cl-incf i))))
          )
      (let (
            (candidates (swiper--candidates nil :advancer1 mc-advancer :initiater1 mc-advancer :use-marker t :use-format-mode-line t))
            )
        (let (
              (res (swiper--ivy candidates initial-input))
              )
          ; (message "after %S res" res)
          res
        )))))

; (add-hook 'minibuffer-exit-hook (lambda () (message "exit")))

(with-eval-after-load 'swiper
  (defalias 'swiper--candidates 'swiper--mcs-candidates)
  (defalias 'swiper--update-input-ivy 'swiper--mcs-update-input-ivy)

  (advice-add 'swiper--ivy :around
              (lambda(orig-fun &rest args)
                (setq mc-swiper--backedup-cursors (mc/store-all-cursors-states))
                (let (
                      (res (apply orig-fun args))
                      )
                  (with-ivy-window
                    (setq mc-swiper--backedup-cursors nil)
                    (unless (= (mc/num-cursors) 1) ; 1 = current cursor
                      (mc/pop-state-from-overlay (car
                                                  (last (mc/all-fake-cursors)))))
                    (mc/maybe-multiple-cursors-mode))
                  res))))

(provide 'multiple-cursors-swiper)
