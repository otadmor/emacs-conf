;; -*- lexical-binding: t; -*-

(with-eval-after-load 'ediff-util
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
        (condition-case nil
            (apply func args)
          (error)))
      (with-selected-window wind-B
        (unless (eq (current-buffer) buff-B)
          (switch-to-buffer ediff-buffer-B))
        (condition-case nil
            (apply func args)
          (error)))
      (when three-way
        (with-selected-window wind-C
          (unless (eq (current-buffer) buff-C)
            (switch-to-buffer ediff-buffer-C))
          (condition-case nil
              (apply func args)
            (error))))
      (when with-Ancestor
        (with-selected-window wind-Anc
          (unless (eq (current-buffer) buff-Anc)
            (switch-to-buffer ediff-ancestor-buffer))
          (condition-case nil
              (apply func args)
            (error))))
      (select-window wind))))

(defun ediff--gather-data (func)
  (let (
        (res nil)
        )
    (ediff-operate-on-windows-func
     (lambda ()
       (add-to-list 'res (funcall func))))
    res))

(defun all-equal (l) (apply '= l))

(defun all (l)
  (every #'identity l))
(defun list-le (l1 l2) (all (mapcar* '<= l1 l2)))
(defun list-ge (l1 l2) (all (mapcar* '>= l1 l2)))

(defun ediff--goto-line (line)
  (ediff-operate-on-windows-func (lambda () (when (/= (line-number-at-pos) line)
                                              (goto-line line)))))


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
          )
      (unless (all-equal post-lines)
          (ediff--goto-line (if (list-be post-lines pre-lines)
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

        (ediff--set-window-start-line (if (list-be post-lines pre-lines) ;;;;;;;;;;;;;;;;;;;;;;;;
                                          (apply 'min post-wind-start)
                                        (apply 'max post-wind-start)))
        ))))

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

(defun ediff--update-overlay-lines (window overlay lines)
  (let* (
         (overlay-start (ediff-overlay-start overlay))
         (overlay-end (ediff-overlay-end overlay))
         (overlay-buffer (ediff-overlay-buffer overlay))
         (overlay-lines (- lines (ediff-overlay-get overlay 'line-count)))
         )
    ;; (with-selected-window window
    (with-current-buffer overlay-buffer
      (save-excursion
        (goto-char overlay-end)
        (dotimes (i overlay-lines)
          (let (
                (overlay-pos (+ overlay-end i))
                )
            (insert "\n")
            (let (
                  (newline-overlay (make-overlay overlay-pos (+ overlay-pos 1)
                                                 overlay-buffer t nil))
                  )
              ;; (overlay-put newline-overlay 'window t)
              ;; (overlay-put newline-overlay 'cursor t)
              ;; (overlay-put newline-overlay 'display (propertize newlines 'cursor t))
              (overlay-put newline-overlay 'display (propertize "...\n"))
              ;; (overlay-put newline-overlay 'after-string (propertize "..."))
              )))))))


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

(defun ediff-clone-file-to-buffer (buf-name file)
  (let (
        (cur-data (with-current-buffer (find-file-noselect file)
                    (buffer-substring (point-min) (point-max))))
        (dir-name (expand-file-name (file-name-directory file)))
        (tmp-buf (new-buffer-frame))
        )
    (with-current-buffer tmp-buf
      (setq default-directory dir-name)
      (rename-buffer (concat (ediff-convert-standard-filename
                              (file-name-nondirectory file)) "-" buf-name))
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
