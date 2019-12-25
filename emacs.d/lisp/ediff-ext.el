;; -*- lexical-binding: t; -*-

(defun ediff-fix-windows-for-buffers(frame-or-window)
  (dolist (window (window-list))
    (with-selected-window window
      (when (ediff-in-control-buffer-p)
        (let* (
               (buff-A ediff-buffer-A)
               (buff-B ediff-buffer-B)
               (buff-C ediff-buffer-C)
               (buff-Anc ediff-ancestor-buffer)
               (wind-A nil)
               (wind-B nil)
               (wind-C nil)
               (wind-Anc nil)
               )
          (dolist (window (window-list))
            (let (
                  (buffer (window-buffer window))
                  )
              (with-selected-window window
                (cond
                 ((eq buff-A buffer) (setq wind-A window))
                 ((eq buff-B buffer) (setq wind-B window))
                 ((eq buff-C buffer) (setq wind-C window))
                 ((eq buff-Anc buffer) (setq wind-Anc window))))))
          (unless (null wind-A) (setq ediff-window-A wind-A))
          (unless (null wind-B) (setq ediff-window-B wind-B))
          (unless (null wind-C) (setq ediff-window-C wind-C))
          (unless (null wind-Anc) (setq ediff-window-Ancestor wind-Anc)))))))

(with-eval-after-load 'ediff-util
  (with-eval-after-load 'persp-mode
    (add-hook 'persp-activated-functions #'ediff-fix-windows-for-buffers))

  (defun ediff-operate-on-windows-func (func &rest args)
    ;; make sure windows aren't dead
    (ediff-barf-if-not-control-buffer)

    (if (not (and (window-live-p ediff-window-A) (window-live-p ediff-window-B)))
        (ediff-recenter 'no-rehighlight))
    (if (not (and (ediff-buffer-live-p ediff-buffer-A)
                  (ediff-buffer-live-p ediff-buffer-B)
                  (or (not ediff-3way-job) (ediff-buffer-live-p ediff-buffer-C))
                  (or (not ediff-merge-with-ancestor-job)
                      (not ediff-show-ancestor)
                      (ediff-buffer-live-p ediff-ancestor-buffer))
                  ))
        (error ediff-KILLED-VITAL-BUFFER))

    (let* ((wind (selected-window))
           (wind-A ediff-window-A)
           (wind-B ediff-window-B)
           (wind-C ediff-window-C)
           (wind-Anc ediff-window-Ancestor)
           (buff-A ediff-buffer-A)
           (buff-B ediff-buffer-B)
           (buff-C ediff-buffer-C)
           (buff-Anc ediff-ancestor-buffer)
           (three-way ediff-3way-job)
           (with-Ancestor (and ediff-merge-with-ancestor-job ediff-show-ancestor)))
      (with-selected-window wind-A
        (unless (eq (current-buffer) buff-A)
          (switch-to-buffer ediff-buffer-A))
        (combine-after-change-calls
          (condition-case nil
              (apply func args)
            (error))))
      (with-selected-window wind-B
        (unless (eq (current-buffer) buff-B)
          (switch-to-buffer ediff-buffer-B))
        (combine-after-change-calls
          (condition-case nil
              (apply func args)
            (error))))
      (when three-way
        (with-selected-window wind-C
          (unless (eq (current-buffer) buff-C)
            (switch-to-buffer ediff-buffer-C))
          (combine-after-change-calls
            (condition-case nil
                (apply func args)
              (error)))))
      (when with-Ancestor
        (with-selected-window wind-Anc
          (unless (eq (current-buffer) buff-Anc)
            (switch-to-buffer ediff-ancestor-buffer))
          (combine-after-change-calls
            (condition-case nil
                (apply func args)
              (error)))))
      (select-window wind))))


(defun ediff-create-lines-database (start end)
  (goto-char start)
  (beginning-of-line)
  (let (
        (new-list nil)
        (last-pos (point))
        )
    (beginning-of-line 2)
    (while (< (point) end)
      (push (- (point) last-pos) new-list)
      (setq last-pos (point))
      (beginning-of-line 2))
    (when (and (= (point) end)
               (/= (point) (point-max)))
      (push (- (point) last-pos) new-list)
      (setq last-pos (point))
      (beginning-of-line 2))
    (push (- (point) last-pos) new-list)
    (reverse new-list)))


(defun ediff-find-in-database (start end)
  (message "start %S end %S" start end)
  (if (null ediff-db)
      (list (cons 0 nil) (cons 0 nil) t t)
    (let* (
           (start-remain (- start 1))
           (end-remain (- end 1))
           (prev nil)
           (prev-line 0)
           (current ediff-db)
           (current-line 1)
           (next (cdr current))
           (next-line 2)
           )
      (when (and (> start-remain 0)
                 (not (null (cdr current))))
        (setq start-remain (- start-remain (car current)))
        (setq end-remain (- end-remain (car current)))
        (while (and (> start-remain 0)
                    (not (null (cdr current))))
          (setq prev current)
          (setq prev-line (+ prev-line 1))
          (setq current (cdr current))
          (setq current-line (+ current-line 1))
          (setq next (cdr current))
          (setq next-line (+ next-line 1))
          (setq start-remain (- start-remain (car current)))
          (setq end-remain (- end-remain (car current))))
        (when (and (= start-remain 0)
                   (not (null (cdr current))))
          (setq prev current)
          (setq prev-line (+ prev-line 1))
          (setq current (cdr current))
          (setq current-line (+ current-line 1))
          (setq next (cdr current))
          (setq next-line (+ next-line 1))))
      (when (and (> end-remain 0)
                 (not (null (cdr current))))
        (setq end-remain (- end-remain (car current)))
        (while (and (> end-remain 0)
                    (not (null (cdr current))))
          (setq current (cdr current))
          (setq current-line (+ current-line 1))
          (setq next (cdr current))
          (setq next-line (+ next-line 1))
          (setq end-remain (- end-remain (car current)))))
      (when (and (/= start end)
                 (= end-remain 0)
                 (not (null (cdr current))))
        (setq current (cdr current))
        (setq current-line (+ current-line 1))
        (setq next (cdr current))
        (setq next-line (+ next-line 1)))
      (message "sr %S er %S" start-remain end-remain)
      (list (cons prev-line prev)
            (cons next-line next)
            (= start-remain -1)
            (= end-remain 0)))))

(defun ediff-after-change-update (start end deleted)
  (save-excursion
    (let* (
           (find-res (ediff-find-in-database start (+ start deleted)))
           (first-res (car find-res))
           (last-res (cadr find-res))
           (first-deleted-before-newline (caddr find-res))
           (last-deleted-after-newline (cadddr find-res))
           (first-line (car first-res))
           (first (cdr first-res))
           (last-line (car last-res))
           (last (cdr last-res))
           (last-insert-is-newline (progn (goto-char end) (bolp)))
           (added-lines (count-lines start end))
           (deleted-lines (- last-line first-line))
           (lines-diff (- added-lines (if (> deleted-lines 2) (- deleted-lines 2) 0)))
           (final-line nil)
           (alignments-to-remove 0)
           )
      (goto-char end)
      (dotimes (i lines-diff)
        (when (null final-line)
          (let (
                (overlay
                 (elisp--find-overlays-specifying 'ediff-alignment-overlay))
                )
            (if (null overlay)
                (setq final-line i)
              (delete-overlay (car overlay))
              (beginning-of-line 2)))))
      (if (null final-line)
          (setq alignments-to-remove (+ lines-diff
                                        ;; (if last-insert-is-newline 1 0)
                                        ))
        (setq alignments-to-remove (+ final-line 1)))
      (when (> (+ alignments-to-remove
                  ;; (if last-deleted-after-newline 1 0)
                  ) 1)
        (message "deleting %S" alignments-to-remove)
        (delete-region end (min (point-max) (+ end alignments-to-remove
                                               ;; (if last-deleted-after-newline 1 0)
                                               -1))))
      (unless (null final-line)
        (goto-char start)
        (let (
              (changed-buffer (current-buffer))
              (add-empty-at-line (+ first-line 1))
              (lines-to-add (- lines-diff alignments-to-remove
                               (if last-insert-is-newline -1 0)))
              (old-window (selected-window))
              )
          (dolist (window (window-list))
            (with-selected-window window
              (when (and (ediff-in-control-buffer-p)
                         (or (eq old-window ediff-window-A)
                             (eq old-window ediff-window-B)
                             (eq old-window ediff-window-C)
                             (eq old-window ediff-window-Ancestor)))
                (ediff-operate-on-windows-func
                 (lambda ()
                   (when (not (eq changed-buffer (current-buffer)))
                     (save-excursion
                       (goto-line add-empty-at-line)
                       (ediff-add-fake-lines (current-buffer)
                                               (point)
                                               lines-to-add))))))))))
      (let (
            (start-for-fake-lines (if first-deleted-before-newline
                                      (line-beginning-position 2)
                                    start))
            (leftover-lines (max 0 (- deleted-lines added-lines 2)))
            )
        (message "leftover-lines %S last-line %S first-line %S first-deleted-before-newline %S" leftover-lines last-line first-line first-deleted-before-newline)
        (when (> leftover-lines 0)
          (ediff-add-fake-lines (current-buffer)
                                start-for-fake-lines
                                leftover-lines))
        (let (
              (new-lines-db (ediff-create-lines-database
                             start
                             (+ end leftover-lines)))
              )
          (message "lines from %S to %S is %S lines" start (+ end leftover-lines) (length new-lines-db))
        (if (null first)
            (if (null new-lines-db)
                (setq ediff-db last)
              (setq ediff-db new-lines-db))
          (if (null new-lines-db)
              (setcdr first last)
            (setcdr first new-lines-db)))
        (unless (null new-lines-db)
          (setcdr (last new-lines-db) last)))))))

(defun ediff--gather-data (func)
  (let (
        (ediff-total-data nil)
        )
    (ediff-operate-on-windows-func
     (lambda ()
       (push (funcall func) ediff-total-data)))
    (reverse ediff-total-data)))

(defun all-equal (l) (apply '= l))

(defun all (l) (every #'identity l))
(defun any (l) (some #'identity l))
(defun list-eq (l1 l2) (all (mapcar* '= l1 l2)))
(defun list-le (l1 l2) (all (mapcar* '<= l1 l2)))
(defun list-ge (l1 l2) (all (mapcar* '>= l1 l2)))

(defun ediff--goto-line (line)
  (ediff-operate-on-windows-func (lambda ()
                                   (let (
                                         (line-beg)
                                         (line-end)
                                         (pos (point))
                                         )
                                     (save-excursion
                                       ;; (- line (line-number-at-pos))
                                       (goto-line line)
                                       (setq line-beg (line-beginning-position))
                                       (setq line-end (line-end-position)))
                                     (if (< pos line-beg)
                                         (goto-char line-beg)
                                       (when (> pos line-end)
                                         (goto-char line-end)))))))


(defun ediff--set-window-start-line (window-start-line)
  (ediff-operate-on-windows-func
   (lambda ()
     (set-window-start (selected-window) (save-excursion
                                           (goto-line window-start-line)
                                           (line-beginning-position))))))

(defun ediff-realign-points (pre-lines)
  (when (all-equal pre-lines)
    (let (
          (post-lines (ediff--gather-data (lambda () (line-number-at-pos))))
          (post-columns (ediff--gather-data (lambda () (current-column))))
          )
      (unless (all-equal post-lines)
        (ediff--goto-line (if (list-ge post-lines pre-lines)
                              (apply 'min post-lines)
                            (when (list-le post-lines pre-lines)
                              (apply 'max post-lines))))
          )
      (let (
            (post-wind-start (ediff--gather-data
                              (lambda ()
                                (save-excursion (goto-char (window-start))
                                                (line-number-at-pos)))))
            )
        (ediff--set-window-start-line (if (list-ge post-lines pre-lines)
                                          (apply 'min post-wind-start)
                                        (apply 'max post-wind-start)))))))

(defun ediff-wrap-interactive (func &optional shift-translated)
  ;; (interactive "P")
  (defalias
    (make-symbol (concat "elisp---" (symbol-name func) "---wrapper"))
    (lambda (&rest args)
      (interactive)
      (let (
            (pre-lines (ediff--gather-data (lambda () (line-number-at-pos))))
            )
        (let (
              (wrapped-func (lambda (&rest wrapped-args)
                              (if shift-translated
                                  (let (
                                        (this-command-keys-shift-translated t)
                                        )
                                    (call-interactively func))
                                (call-interactively func))))
              )
          (apply 'ediff-operate-on-windows-func
                 (cons wrapped-func args)))
        (ediff-realign-points pre-lines))
      (ediff-fix-mark))
    (documentation func)))


(defun ediff-fix-mark ()
  (ediff-operate-on-windows-func
   (lambda ()
     (let (
           (current-point (point))
           (current-mark (and (region-active-p) (mark t)))
           (is-eol (eolp))
           )
       (lockstep--remove-fake-cursors)
       (lockstep--create-fake-cursor-and-region is-eol current-mark current-point)))))

(defun ediff-add-fake-lines (buffer pos lines)
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (insert (make-string lines ?\n))
      (dotimes (i lines)
        (let (
              (at-pos (+ pos i))
              )
          (goto-char at-pos)
          (let (
                (newline-overlay (make-overlay at-pos (+ at-pos 1) buffer t nil))
                )
            ;; (overlay-put newline-overlay 'window t)
            ;; (overlay-put newline-overlay 'cursor t)
            ;; (overlay-put newline-overlay 'display (propertize newlines 'cursor t))
            (overlay-put newline-overlay 'display (propertize "...\n"))
            (overlay-put newline-overlay 'ediff-alignment-overlay t)
            ;; (overlay-put newline-overlay 'after-string (propertize "..."))
            ))))))

(defun ediff--update-overlay-lines (window overlay lines)
  (let* (
         (overlay-start (ediff-overlay-start overlay))
         (overlay-end (ediff-overlay-end overlay))
         (overlay-buffer (ediff-overlay-buffer overlay))
         (overlay-lines (- lines (ediff-overlay-get overlay 'line-count)))
         )
    ;; (with-selected-window window
    (ediff-add-fake-lines overlay-buffer overlay-end overlay-lines)))


(defun ediff--get-diff-lines-from-overlay (overlay)
  (with-current-buffer (ediff-overlay-buffer overlay)
    (let (
          (lines (count-lines (ediff-overlay-start overlay)
                              (ediff-overlay-end overlay)))
          )
      (ediff-overlay-put overlay 'line-count lines)
      lines)))

(defun ediff--extend-lines (orig-fun &rest args)
  (let (
        (res (apply orig-fun args))
        (diff-amount ediff--diff-amount)
        )
    (dotimes (current-diff (- diff-amount 1))
      (let (
            (overlay-A (ediff-get-diff-overlay current-diff 'A))
            (overlay-B (ediff-get-diff-overlay current-diff 'B))
            (overlay-C (when ediff-3way-job
                         (ediff-get-diff-overlay current-diff 'C)))
            (overlay-Anc (when ediff-merge-with-ancestor-job
                           (ediff-get-diff-overlay current-diff 'Ancestor)))
            )
        (ediff-overlay-put overlay-A 'window t)
        (ediff-overlay-put overlay-B 'window t)
        (unless (null overlay-C)
          (ediff-overlay-put overlay-C 'window t))
        (unless (null overlay-Anc)
          (ediff-overlay-put overlay-Anc 'window t))
        (let (
              (lines-A (ediff--get-diff-lines-from-overlay overlay-A))
              (lines-B (ediff--get-diff-lines-from-overlay overlay-B))
              (lines-C (if (null overlay-C)
                           0
                         (ediff--get-diff-lines-from-overlay overlay-C)))
              (lines-Anc (if (null overlay-Anc)
                             0
                           (ediff--get-diff-lines-from-overlay overlay-Anc)))
              )
          (let (
                (max-lines (max lines-A lines-B lines-C lines-Anc))
                )
            (ediff--update-overlay-lines ediff-window-A overlay-A max-lines)
            (ediff--update-overlay-lines ediff-window-B overlay-B max-lines)
            (unless (null overlay-C)
              (ediff--update-overlay-lines ediff-window-C overlay-C max-lines))
            (unless (null overlay-Anc)
              (ediff--update-overlay-lines ediff-window-Ancestor overlay-Anc max-lines))))))
    res))

(defun ediff--count-diffs (orig-fun &rest args)
  (let (
        (res (apply orig-fun args))
        (diff-amount (length (car args)))
        )
    (setq ediff--diff-amount diff-amount)
    res))


(defun ediff--load-faces ()
    (dolist (entry '((ediff-current-diff-C . ((((class color) (background light))
                                             (:background "#DDEEFF" :foreground "#005588"))
                                            (((class color) (background dark))
                                             (:background "#005588" :foreground "#DDEEFF"))))
                   (ediff-fine-diff-C . ((((class color) (background light))
                                          (:background "#EEFFFF" :foreground "#006699"))
                                         (((class color) (background dark))
                                          (:background "#006699" :foreground "#EEFFFF"))))))
    (let ((face (car entry))
          (spec (cdr entry)))
      (put face 'theme-face nil)
      (face-spec-set face spec)))

  (dolist (face-map '((diff-hl-insert              . magit-diff-added)
                      (diff-hl-change              . ediff-current-diff-C)
                      (diff-hl-delete              . magit-diff-removed)
                      (smerge-base                 . magit-diff-base)
                      (smerge-lower                . magit-diff-added)
                      (smerge-markers              . magit-diff-conflict-heading)
                      (smerge-refined-added        . magit-diff-added-highlight)
                      (smerge-refined-removed      . magit-diff-removed-highlight)
                      (smerge-upper                . magit-diff-removed)
                      (ediff-even-diff-A           . magit-diff-context-highlight)
                      (ediff-even-diff-Ancestor    . magit-diff-context)
                      (ediff-even-diff-B           . magit-diff-context-highlight)
                      (ediff-even-diff-C           . magit-diff-context-highlight)
                      (ediff-odd-diff-A            . magit-diff-context-highlight)
                      (ediff-odd-diff-Ancestor     . magit-diff-context)
                      (ediff-odd-diff-B            . magit-diff-context-highlight)
                      (ediff-odd-diff-C            . magit-diff-context-highlight)
                      (ediff-current-diff-A        . magit-diff-our)
                      (ediff-current-diff-Ancestor . magit-diff-base)
                      (ediff-current-diff-B        . magit-diff-their)
                      (ediff-fine-diff-A           . magit-diff-removed-highlight)
                      (ediff-fine-diff-Ancestor    . magit-diff-base-highlight)
                      (ediff-fine-diff-B           . magit-diff-added-highlight)
                      (diff-header                 . magit-diff-hunk-heading)
                      (diff-context                . magit-diff-context)
                      (diff-added                  . magit-diff-added)
                      (diff-removed                . magit-diff-removed)
                      (diff-changed                . smerge-refined-changed)
                      (diff-refine-added           . magit-diff-added-highlight)
                      (diff-refine-removed         . magit-diff-removed-highlight)
                      (diff-refine-changed         . ediff-fine-diff-C)
                      (diff-indicator-added        . magit-diffstat-added)
                      (diff-indicator-removed      . magit-diffstat-removed)))
    (let* ((face (car face-map))
           (alias (cdr face-map)))
      (put face 'theme-face nil)
      (put face 'face-alias alias))))

(defun elisp--find-overlays-specifying (prop)
  (let ((overlays (overlays-at (point)))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun ediff-is-current-alignment-overlay ()
  (not (null (elisp--find-overlays-specifying 'ediff-alignment-overlay))))

(defun ediff-save-file (buffer file)
  (with-current-buffer (orig-buffer (find-file-noselect file))
    (let (
          (new-text (with-temp-buffer ; calculate this before delete-region
                      (let (
                            (tmp-buffer (current-buffer))
                            )
                        (with-current-buffer buffer
                          (save-excursion
                            (goto-char (point-min))
                            (while (< (point) (point-max))
                              (when (not (ediff-is-current-alignment-overlay))
                                (let (
                                      (current-line (thing-at-point 'line))
                                      )
                                  (with-current-buffer tmp-buffer
                                    (insert current-line))))
                              (forward-line))
                            (with-current-buffer tmp-buffer
                              (buffer-substring (point-min) (point-max))))))))
          )
      (delete-region (point-min) (point-max))
      (insert new-text)
      (save-buffer)))
  t) ; return true to stop saving in write-contents-functions hook.

(defun ediff-clone-file-to-buffer (buf-name file)
  (let* (
         (file (expand-file-name file))
         (cur-data (with-current-buffer (find-file-noselect file)
                     (buffer-substring (point-min) (point-max))))
         (dir-name (expand-file-name (file-name-directory file)))
         (tmp-buf (generate-new-buffer
                   (concat (ediff-convert-standard-filename
                            (file-name-nondirectory file)) "-" buf-name)))
         )
    (add-to-list 'ediff--temporary-buffers tmp-buf)
    (with-current-buffer tmp-buf
      (defvar-local ediff-db nil)
      (add-hook 'write-contents-functions (lambda () (ediff-save-file tmp-buf file)))
      (add-hook 'after-change-functions #'ediff-after-change-update nil t)
      (setq default-directory dir-name)
      (toggle-truncate-lines 1)
      (insert cur-data))
    tmp-buf))

(defun ediff-files-internal-using-buffers (file-A file-B file-C
                                                  startup-hooks job-name
                                                  &optional merge-buffer-file)
  (let (
        (buf-A (ediff-clone-file-to-buffer "A" file-A))
        (buf-B (ediff-clone-file-to-buffer "B" file-B))
        (buf-C (unless (null file-C) (ediff-clone-file-to-buffer "C" file-C)))
        )
  (ediff-buffers-internal buf-A buf-B buf-C
                          startup-hooks job-name
                          merge-buffer-file)))


;; (defun ediff-find-file-as-buffer (file-var buffer-name &optional last-dir hooks-var)
;;   (let* (
;;          (file--hook (symbol-value file-var))
;;          )
;;     (cond ((not (file-readable-p file--hook))
;; 	   (user-error "File `%s' does not exist or is not readable" file--hook))
;; 	  ((file-directory-p file--hook)
;; 	   (user-error "File `%s' is a directory" file--hook)))

;;     ;; some of the commands, below, require full file name
;;     (setq file--hook (expand-file-name file--hook))

;;     ;; Record the directory of the file
;;     (if last-dir
;; 	(set last-dir (expand-file-name (file-name-directory file--hook))))

;;     (message "ediff find file %S %S" file--hook buffer-name)
;;     ;; Setup the buffer
;;     (set buffer-name (ediff-clone-file-to-buffer buffer-name file--hook))

;;     (ediff-with-current-buffer (symbol-value buffer-name)
;;       (set hooks-var (cons `(lambda () (delete-file ,file--hook))
;; 				  (symbol-value hooks-var))))))

(setq ediff--temporary-buffers nil)
(defun ediff--kill-all-buffers ()
  (dolist (buffer ediff--temporary-buffers)
    (kill-buffer buffer)))

;; (add-hook 'ediff-load-hook
(with-eval-after-load 'ediff
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-toggle-multiframe nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  (ediff--load-faces)

  ;; (defalias 'ediff-find-file 'ediff-find-file-as-buffer)
  (defalias 'ediff-files-internal 'ediff-files-internal-using-buffers)
  (advice-add 'ediff-convert-diffs-to-overlays :around 'ediff--count-diffs)
  (advice-add 'ediff-setup-windows :around 'ediff--extend-lines)
  (add-hook 'ediff-quit-hook #'ediff--kill-all-buffers)

  (add-hook
   'ediff-keymap-setup-hook
   '(lambda ()
      (setq ediff-mode-map (make-keymap))
      (or (char-table-p (nth 1 ediff-mode-map))
          (error "The initialization of isearch-mode-map must be updated"))
      (define-key ediff-mode-map (kbd "<right>") (ediff-wrap-interactive #'right-char))
      (define-key ediff-mode-map (kbd "<left>") (ediff-wrap-interactive #'left-char))
      (define-key ediff-mode-map (kbd "<up>") (ediff-wrap-interactive #'previous-line))
      (define-key ediff-mode-map (kbd "<down>") (ediff-wrap-interactive #'next-line))
      (define-key ediff-mode-map (kbd "<next>") (ediff-wrap-interactive #'scroll-up-command))
      (define-key ediff-mode-map (kbd "<prior>") (ediff-wrap-interactive #'scroll-down-command))
      (define-key ediff-mode-map (kbd "<home>") (ediff-wrap-interactive #'move-beginning-of-line))
      (define-key ediff-mode-map (kbd "<end>") (ediff-wrap-interactive #'move-end-of-line))
      (define-key ediff-mode-map (kbd "C-<home>") (ediff-wrap-interactive #'beginning-of-buffer))
      (define-key ediff-mode-map (kbd "C-<end>") (ediff-wrap-interactive #'end-of-buffer))
      (define-key ediff-mode-map (kbd "M-<left>") (ediff-wrap-interactive #'backward-sexp))
      (define-key ediff-mode-map (kbd "M-<right>") (ediff-wrap-interactive #'forward-sexp))

      (define-key ediff-mode-map (kbd "S-<right>") (ediff-wrap-interactive #'right-char t))
      (define-key ediff-mode-map (kbd "S-<left>") (ediff-wrap-interactive #'left-char t))
      (define-key ediff-mode-map (kbd "S-<up>") (ediff-wrap-interactive #'previous-line t))
      (define-key ediff-mode-map (kbd "S-<down>") (ediff-wrap-interactive #'next-line t))


      (define-key ediff-mode-map [backspace] (ediff-wrap-interactive #'backward-delete-char-untabify))
      (define-key ediff-mode-map "\177" (ediff-wrap-interactive #'delete-forward-char))
      (define-key ediff-mode-map (kbd "<deletechar>") (ediff-wrap-interactive #'delete-forward-char)) ; <delete>
      (define-key ediff-mode-map "\r" (ediff-wrap-interactive #'newline))
      (define-key ediff-mode-map "<return>" (ediff-wrap-interactive #'newline))
      (define-key ediff-mode-map [return] (ediff-wrap-interactive #'newline))
      (define-key ediff-mode-map (kbd "<tab>") (ediff-wrap-interactive #'indent-for-tab-command))

      (define-key ediff-mode-map [(control z)] (ediff-wrap-interactive #'undo))
      (define-key ediff-mode-map [(control y)] (ediff-wrap-interactive #'redo))

      (define-key ediff-mode-map (kbd "C-<insert>") (ediff-wrap-interactive #'kill-ring-save))
      (define-key ediff-mode-map (kbd "S-<insert>") (ediff-wrap-interactive #'yank))
      (define-key ediff-mode-map (kbd "S-<delete>") (ediff-wrap-interactive #'kill-region))

      (define-key ediff-mode-map (kbd "C-x C-s") (ediff-wrap-interactive #'save-buffer))

      (set-char-table-range (nth 1 ediff-mode-map) (cons #x100 (max-char))
                            (ediff-wrap-interactive #'self-insert-command))
      (let (
            (i 0)
            )
        (setq i ?\s)
        (while (< i 256)
          (define-key ediff-mode-map (vector i)
            (ediff-wrap-interactive #'self-insert-command))
          (setq i (1+ i))))

      (define-key ediff-mode-map (kbd "C-n") #'ediff-next-difference)
      (define-key ediff-mode-map (kbd "C-p") #'ediff-previous-difference)

      (define-key ediff-mode-map (kbd "C-?") #'ediff-toggle-help)

      (define-key ediff-mode-map (kbd "C-M-k") #'ediff-quit)
      ))
  )

(provide 'ediff-ext)
