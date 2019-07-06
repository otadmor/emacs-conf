;;;  -*- lexical-binding: t -*-

(require 'auto-complete)
(ac-config-default)
(setq-default ac-sources '(ac-source-abbrev ac-source-dictionary))
;; (global-auto-complete-mode t)
;; (define-globalized-minor-mode real-global-auto-complete-mode
;;   auto-complete-mode (lambda ()
;;                        (if (not (minibufferp (current-buffer)))
;;                          (auto-complete-mode 1))
;;                        ))
;; (real-global-auto-complete-mode t)
(setq tab-always-indent 'complete)
(setq ac-max-width 0.5)
;; (setq ac-use-fuzzy nil)
(setq ac-use-fuzzy t)
(setq ac-ignore-case (quote smart))

(defun ac-page-next ()
  "Select next candidate."
  (interactive)
  (when (ac-menu-live-p)
    (when (popup-hidden-p ac-menu)
      (ac-show-menu))
    (popup-page-next ac-menu)
    (if (eq this-command 'ac-page-next)
        (setq ac-dwim-enable t))))

(defun ac-page-previous ()
  "Select previous candidate."
  (interactive)
  (when (ac-menu-live-p)
    (when (popup-hidden-p ac-menu)
      (ac-show-menu))
    (popup-page-previous ac-menu)
    (if (eq this-command 'ac-page-previous)
        (setq ac-dwim-enable t))))

(defun ac-first ()
  (interactive)
  (when (ac-menu-live-p)
    (when (popup-hidden-p ac-menu)
      (ac-show-menu))
    (popup-jump ac-menu 0)
    (if (eq this-command 'ac-first)
        (setq ac-dwim-enable t))))

(defun ac-last ()
  (interactive)
  (when (ac-menu-live-p)
    (when (popup-hidden-p ac-menu)
      (ac-show-menu))
    (let (
          (popup-length (length (popup-list ac-menu)))
          (popup-height (popup-height ac-menu))
          )
      (when (> popup-length popup-height)
        (setf (popup-scroll-top ac-menu) (- popup-length popup-height)))
      (setf (popup-cursor ac-menu) (- popup-length 1)))
    (popup-draw ac-menu)
    (if (eq this-command 'ac-last)
        (setq ac-dwim-enable t))))


(defun auto-complete-completion-in-region (start end collection &optional predicate)
  (let* (
         (fixed-candidate-source
          (list
           (list 'candidates (lambda ()
                               (let (
                                     (cands
                                      (completion-all-completions
                                       (buffer-substring-no-properties start end)
                                       collection predicate
                                       (- end start))
                                      )
                                     )
                                 (unless (null cands)
                                   (setcdr (last cands) nil))
                                 (dolist (s cands)
                                   (ivy--remove-props s 'face))
                                 cands)))
           (list 'prefix (lambda () start))))
         )
    (auto-complete (list fixed-candidate-source))))

(defvar ac-original-point nil
  "Stores the original ac-point for relocation use.")
(defun ac-clear-ac-original-point (&rest args)
  (setq ac-original-point nil))
(advice-add 'ac-start :before #'ac-clear-ac-original-point)
(defun ac-candidates-1-reposition-hook (orig-fun &rest args)
  (let (
        (start-pos nil)
        (start 0)
        (cands (apply orig-fun args))
        )
    (unless (null cands)
      (dolist (c cands)
        (let (
              (pos (get-text-property 0 :pos c))
              )
          (unless (null pos)
            (if (null start-pos)
                (setq start-pos pos)
              (setq start-pos (min start-pos pos))))))
      (when (null start-pos)
        (setq start-pos 0))
      (setq cands (cl-mapcar
                   (lambda (s)
                     ;; (ivy--remove-props s 'face)
                     (substring s start-pos nil))
                   cands))
      (when (null ac-original-point)
        (setq ac-original-point ac-point))
      (setq ac-point (+ ac-original-point start-pos))
      (setq ac-prefix (buffer-substring-no-properties ac-point (point))))
    cands))
(advice-add 'ac-candidates-1 :around #'ac-candidates-1-reposition-hook)

(defun completion-in-region-auto-complete-or-ivy (start end collection &optional predicate)
  (if (eq (selected-window) (active-minibuffer-window))
    (ivy-completion-in-region start end collection predicate)
  (auto-complete-completion-in-region start end collection predicate)))


(defvar ac-default-min-prefix-length 0
  "The minimum prefix requirement for completing using auto-complete. Can be determined per-source by setting requires.")
(defun ac-prefix (requires ignore-list)
  (cl-loop with current = (point)
           with point
           with point-def
           with prefix-def
           with sources
           for source in (ac-compiled-sources)
           for prefix = (assoc-default 'prefix source)
           for req = (or (assoc-default 'requires source) requires ac-default-min-prefix-length)

           do
           (unless (member prefix ignore-list)
             (save-excursion
               (setq point (cond
                            ((symbolp prefix)
                             (funcall prefix))
                            ((stringp prefix)
                             (and (re-search-backward (concat prefix "\\=") nil t)
                                  (or (match-beginning 1) (match-beginning 0))))
                            ((stringp (car-safe prefix))
                             (let ((regexp (nth 0 prefix))
                                   (end (nth 1 prefix))
                                   (group (nth 2 prefix)))
                               (and (re-search-backward (concat regexp "\\=") nil t)
                                    (funcall (if end 'match-end 'match-beginning)
                                             (or group 0)))))
                            (t
                             (eval prefix))))
               (if (and point
                        (integerp req)
                        (< (- current point) req))
                   (setq point nil))
               (when point
                 (if (null prefix-def)
                     (setq prefix-def prefix
                           point-def point))
                 (if (equal point point-def)
                     (push source sources)))))

           finally return
           (and point-def (list prefix-def point-def (nreverse sources)))))

;; (setq ac-expand-on-auto-complete nil)

(defun ac-complete-when-menu ()
  (interactive)
  (if ac-show-menu
      (ac-complete)
    (ac-abort)
    (ac-fallback-command)))

(provide 'autocomplete-ext)
