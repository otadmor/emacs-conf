;; -*- lexical-binding: t; -*-
(require 'swiper)
(require 'ivy)
(require 'multiple-cursors-swiper) ; for candidates with advancer and initiater

(setq swiper-include-line-number-in-search nil)
(defun swiper-line-transformer (str)
  (concat (format (swiper--async-format-spec)
                  (let (
                        (pos (swiper--get-end str))
                        )
                    (if pos (save-excursion (goto-char pos) (line-number-at-pos))
                      (swiper--get-line str)))) str))
(ivy-set-display-transformer 'swiper 'swiper-line-transformer)


(defun swiper--async-action(orig-fun &rest args)
  (let (
        (x (car args))
        )
    (let (
          (pos (swiper--get-end x))
          )
      (when pos
        (let (
              (line-no (save-excursion (goto-char pos) (line-number-at-pos)))
              )
          (let (
                (line-number-str (format swiper--format-spec line-no))
                )
            (put-text-property
             0 1 'swiper-line-number line-number-str x))))))
  (apply orig-fun args))
(advice-add 'swiper--action :around 'swiper--async-action)


(defcustom swiper-async-filter-update-time 50
  "The amount of microseconds to wait until updating `swiper--async-filter'."
  :type 'integer)

(defun swiper--get-line (item)
  (get-text-property 0 'swiper-no-line-number item))
(defun swiper--get-str-line (item)
  (get-text-property 0 'swiper-line-number item))
(defun swiper--get-region (item)
  (get-text-property 0 'region-data item))
(defun swiper--get-begin (item)
  (cadr (swiper--get-region item)))
(defun swiper--get-end (item)
  (car (swiper--get-region item)))
(defun swiper--get-line-region (item)
  (get-text-property 0 'line-region-data item))
(defun swiper--get-line-begin (item)
  (cadr (swiper--get-line-region item)))
(defun swiper--get-line-end (item)
  (car (swiper--get-line-region item)))


(defun swiper--line-with-borders ()
  (let* ((beg (cond ((and (eq major-mode 'dired-mode)
                          (bound-and-true-p dired-isearch-filenames))
                     (dired-move-to-filename)
                     (point))
                    (swiper-use-visual-line
                     (save-excursion
                       (beginning-of-visual-line)
                       (point)))
                    (t
                     (point))))
         (end (if swiper-use-visual-line
                  (save-excursion
                    (end-of-visual-line)
                    (point))
                (line-end-position))))
    (concat " " (buffer-substring beg end))))
(defalias 'swiper--line 'swiper--line-with-borders)

(defvar ivy--orig-cands nil
  "Store the original candidates found.")

(defun swiper--async-iterate-matches (pattern beg end func)
  (when (and (/= (length pattern) 0) (< beg end))
    (goto-char beg)
    (while (word-search-forward-lax
            pattern
            end
            'on-error-go-to-limit)
      (funcall func (match-beginning 0) (match-end 0)))))

(defun swiper--async-filter (buffer change-begin inserted-end deleted-length)
  "Receive from buffer the output STR.
Update the minibuffer with the amount of lines collected every
`swiper-async-filter-update-time' microseconds since the last update."
  (when (and t
             ;; (time-less-p (list 0 0 swiper-async-filter-update-time)
             ;; (time-since counsel--async-time))
             )
    (let (
          (working-candidates ivy--orig-cands)
          (chars-diff (- (- inserted-end change-begin) deleted-length))
          (deleted-end (+ change-begin deleted-length))
          (deleted-matches 0)
          (found-matches 0)
          (first-item nil)
          (last-item nil)
          )
      (let (
            (change-end (max deleted-end inserted-end))
            )
        (let (
              (iterator working-candidates)
              )
          (while iterator
            (let (
                  (item (car iterator))
                  )
              (when (<= (swiper--get-line-begin item) change-end)
                (cl-incf deleted-matches)
                (setq last-item iterator))
              (when (< (swiper--get-line-end item) change-begin)
                (cl-decf deleted-matches)
                (setq first-item iterator)))
            (setq iterator (cdr iterator))))
        (setq last-item (if (null last-item) ivy--orig-cands (cdr last-item)))
        (let (
              (search-start (progn (goto-char change-begin) (line-beginning-position)))
              (search-end (progn (goto-char change-end) (line-end-position)))
              (new-candidates)
              (new-candidates-tail)
              )
          (swiper--async-iterate-matches
           ivy-text search-start search-end
           (let (
                 (format-spec (swiper--async-format-spec))
                 )
             (lambda (b e)
               (cl-incf found-matches)
               (let (
                     (new-item (list (swiper--async-create-candidate format-spec b e)))
                     )
                 (if (null new-candidates)
                     (progn
                       (setq new-candidates-tail new-item)
                       (setq new-candidates new-item))
                   (setcdr new-candidates-tail new-item)
                   (setq new-candidates-tail new-item))))))
          (let (
                (tail-items (if (null new-candidates)
                                last-item
                              (setcdr new-candidates-tail last-item)
                              new-candidates))
                )
            (if (null first-item)
                (setq ivy--orig-cands tail-items)
              (setcdr first-item tail-items)))))
      (when (/= (+ found-matches deleted-matches) 0)
        (swiper--async-update-output)))
    (setq counsel--async-time (current-time))))

(defun swiper-async-after-change(begin end deleted-length)
  (save-excursion
    (swiper--async-filter (current-buffer) begin end deleted-length)))

(setq to-search nil)
(setq isearch-swiper-limit 3)
(defun swiper-async-function (string)
  "Grep in the current directory for STRING."
  ;; (counsel--elisp-to-pcre (setq ivy--old-re (ivy--regex string)))
  (setq isearch-string ivy-text)
  (when (and (/= (length ivy-text) 0)
             (or (<= (length ivy-text) isearch-swiper-limit)
                 (= (length to-search) 0)
                 (not (string-prefix-p to-search ivy-text))))
    (setq to-search (if (< (length ivy-text) 3) ivy-text (substring ivy-text 0 isearch-swiper-limit)))
      ; (= (length ivy--orig-cands) 0)
    (setq ivy--old-cands nil)
    (setq ivy--all-candidates nil)
    (setq ivy--orig-cands nil)
    (setq ivy--index 0)
    (swiper--async-init))
  (let (
        (res (if (string= ivy-text "")
                 ivy--orig-cands
               (ivy--filter ivy-text ivy--orig-cands)))
        )
    (swiper--async-update-input-ivy)
    res))

(defun swiper-async--cleanup ()
  (with-ivy-window
    (setq to-search nil)
    (lazy-highlight-cleanup t)
    (isearch-dehighlight)
    (remove-hook 'after-change-functions #'swiper-async-after-change t)
    (remove-hook 'window-scroll-functions #'swiper--async-update-input-ivy-scroll-hook t)
    (remove-hook 'window-size-change-functions #'swiper--async-update-input-ivy-size-hook t)
    ; (remove-hook 'window-configuration-change-hook #'swiper--async-update-input-ivy-hook t)
    )
  (swiper--cleanup))

(defun swiper-async (&optional initial-input)
  "`isearch' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (swiper--async-ivy initial-input))


(setq swiper--async-timer nil)
(defun schedule-isearch(buffer func)
  (setq swiper--async-timer (run-at-time
                             swiper--async-isearch-interval
                             nil 'swiper--async-isearch buffer func)))

(setq swiper--async-isearch-interval 0)
(setq swiper--async-high-start-point nil)
(setq swiper--async-high-end-point nil)
(setq swiper--async-low-start-point nil)
(setq swiper--async-low-end-point nil)
(setq swiper--async-direction-backward nil)
(setq swiper--async-max-matches-per-search 10)
(setq swiper--async-default-max-matches-per-search 100)
(setq swiper--max-search-length 4096) ; one page?

(defun swiper--async-update-output ()
  (ivy--set-candidates ivy--orig-cands)
  (let (
        (this-command 'swiper-async)
        )
    (setq ivy--old-re nil) ; force recalculation
    (ivy--insert-minibuffer
     (ivy--format
      (if (string= ivy-text "")
          ivy--orig-cands
        (ivy--filter ivy-text ivy--orig-cands))))
    (swiper--async-update-input-ivy)))

(defun swiper--async-isearch(buffer func)
  (when (and (/= (length to-search) 0)
             (active-minibuffer-window)
             )
    (let (
          (matches-found 0)
          )
      (with-ivy-window
        (save-excursion
          (deactivate-mark)
          (let (
                (swiper--async-max-matches-per-search
                 (if (< (length ivy--orig-cands) (+ ivy-height ivy--index))
                     (- (+ ivy-height ivy--index) (length ivy--orig-cands))
                   swiper--async-default-max-matches-per-search))
                )
            (if (not swiper--async-direction-backward)
                (progn
                  (when (< swiper--async-high-start-point
                           swiper--async-high-end-point)
                    (goto-char swiper--async-high-start-point)
                    (while (and (not (input-pending-p))
                                (< matches-found swiper--async-max-matches-per-search)
                                (word-search-forward-lax
                                 to-search
                                 (min swiper--async-high-end-point
                                      (+ swiper--async-high-start-point
                                         swiper--max-search-length
                                         1))
                                 'on-error-go-to-limit))
                      (cl-incf matches-found)
                      (if (>= ivy--index
                              (funcall func (match-beginning 0) (match-end 0)))
                          (cl-incf ivy--index)))
                    (setq swiper--async-high-start-point (point)))
                  (when (< swiper--async-low-start-point
                           swiper--async-low-end-point)
                    (goto-char swiper--async-low-start-point)
                    (while (and (not (input-pending-p))
                                (< matches-found swiper--async-max-matches-per-search)
                                (word-search-forward-lax
                                 to-search
                                 (min swiper--async-low-end-point
                                      (+ swiper--async-low-start-point
                                         swiper--max-search-length
                                         1))
                                 'on-error-go-to-limit))
                      (cl-incf matches-found)
                      (if (>= ivy--index
                              (funcall func (match-beginning 0) (match-end 0)))
                          (cl-incf ivy--index)))
                    (setq swiper--async-low-start-point (point))))
              (progn
                (when (< swiper--async-low-start-point
                         swiper--async-low-end-point)
                  (goto-char swiper--async-low-end-point)
                  (while (and (not (input-pending-p))
                              (< matches-found swiper--async-max-matches-per-search)
                              (word-search-backward-lax
                               to-search
                               (max swiper--async-low-start-point
                                    (- swiper--async-low-end-point
                                       swiper--max-search-length
                                       1))
                               'on-error-go-to-limit))
                    (cl-incf matches-found)
                    (if (>= ivy--index
                            (funcall func (match-beginning 0) (match-end 0)))
                        (cl-incf ivy--index)))
                  (setq swiper--async-low-end-point (point)))
                (when (< swiper--async-high-start-point
                         swiper--async-high-end-point)
                  (goto-char swiper--async-high-end-point)
                  (while (and (not (input-pending-p))
                              (< matches-found swiper--async-max-matches-per-search)
                              (word-search-backward-lax
                               to-search
                               (max swiper--async-high-start-point
                                    (- swiper--async-high-end-point
                                       swiper--max-search-length
                                       1))
                               'on-error-go-to-limit))
                    (cl-incf matches-found)
                    (if (>= ivy--index
                            (funcall func (match-beginning 0) (match-end 0)))
                        (cl-incf ivy--index)))
                  (setq swiper--async-high-end-point (point))))))))
      (when (/= matches-found 0)
        (swiper--async-update-output)))
    (when (or (< swiper--async-high-start-point
                 swiper--async-high-end-point)
              (< swiper--async-low-start-point
                 swiper--async-low-end-point))
      (schedule-isearch buffer func))))

(defun swiper--async-insertion-sort (candidate comp-func)
  (let (
        (idx 0)
        )
    (if (null ivy--orig-cands)
        (setq ivy--orig-cands (list candidate))
      (if (funcall comp-func candidate (car ivy--orig-cands))
          (push candidate ivy--orig-cands)
        (let (
              (insertion-point ivy--orig-cands)
              )
          (while (and
                  (not (null (cdr insertion-point)))
                  (not (funcall comp-func candidate (cadr insertion-point))))
            (cl-incf idx)
            (setq insertion-point (cdr insertion-point)))
          (let (
                (current-cdr (cdr insertion-point))
                (current-candidate (list candidate))
                )
            (setcdr insertion-point current-candidate)
            (setcdr current-candidate current-cdr)))))
  idx))

(defun candidate--compare (c1 c2)
  (let (
        (l1 (swiper--get-line c1))
        (l2 (swiper--get-line c2))
        )
    (or (< l1 l2)
        (and (= l1 l2)
             (< (swiper--get-end c1)
                (swiper--get-end c2))))))

(defun swiper--async-create-candidate (format-spec b e)
  (save-restriction
    (widen)
    (let (
          (lb (save-excursion (goto-char b) (point-at-bol)))
          (le (save-excursion (goto-char e) (point-at-eol)))
          )
      (swiper--fill-candidate-properties
       (buffer-substring lb le)
       format-spec
       0
       t b e lb le))))

(defun swiper--async-found-new-candidate (format-spec b e)
  (swiper--async-insertion-sort
   (swiper--async-create-candidate format-spec b e)
   'candidate--compare))

(defun swiper--async-format-spec ()
  (let* ((n-lines (count-lines (point-min) (point-max))))
    (setq swiper--width (1+ (floor (log n-lines 10))))
    (setq swiper--format-spec (format "%%-%dd: " swiper--width))))


(defun swiper--async-init ()
  (setq counsel--async-time (current-time))
  (setq counsel--async-start counsel--async-time)
  (with-ivy-window
    (setq swiper--async-high-start-point swiper--opoint)
    (setq swiper--async-high-end-point (point-max))
    (setq swiper--async-low-start-point (point-min))
    (setq swiper--async-low-end-point swiper--async-high-start-point)
    (when (not (null swiper--async-timer))
      (cancel-timer swiper--async-timer)
      (setq swiper--async-timer nil))
    (schedule-isearch
     (current-buffer)
     (let (
           (swiper--format-spec (swiper--async-format-spec))
           )
       (lambda (b e)
         (swiper--async-found-new-candidate swiper--format-spec b e))))))


(defun swiper--async-mark-candidates-in-window ()
  (with-ivy-window
    (swiper--async-mark-candidates-in-range
     (max
      (if (display-graphic-p)
          (window-start)
        (line-beginning-position (- (window-height))))
      swiper--point-min)
     (min
      (if (display-graphic-p)
          (window-end (selected-window) t)
        (line-end-position (window-height)))
      swiper--point-max))))

(defun swiper--async-mark-candidates-in-range (beg end)
  ;; (save-excursion (swiper--add-overlays ivy-text beg end))
  ;; (save-excursion (isearch-lazy-highlight-new-loop beg end))
  (save-excursion
    (lazy-highlight-cleanup t)
    (swiper--async-iterate-matches
     ivy-text beg end
     'swiper--async-mark-candidate)))

(defun swiper--async-mark-candidate (beg end)
  (let (
        (isearch-lazy-highlight-buffer (current-buffer))
        )
    (isearch-lazy-highlight-match beg end)))

(defvar isearch-overlay nil)

(defun isearch-highlight (beg end)
  (if search-highlight
      (if isearch-overlay
	  ;; Overlay already exists, just move it.
	  (move-overlay isearch-overlay beg end (current-buffer))
	;; Overlay doesn't exist, create it.
	(setq isearch-overlay (make-overlay beg end))
	;; 1001 is higher than lazy's 1000 and ediff's 100+
	(overlay-put isearch-overlay 'priority 1001)
	(overlay-put isearch-overlay 'face isearch-face))))

(defun isearch-dehighlight ()
  (when isearch-overlay
    (delete-overlay isearch-overlay)))

;; (setq disable-point-adjustment t)
;; (run-hooks 'isearch-update-post-hook)
;; (setq cursor-sensor-inhibit (delq 'isearch cursor-sensor-inhibit))

(setq isearch-lazy-highlight-buffer nil)
(defun isearch-lazy-highlight-match (mb me)
  (let ((ov (make-overlay mb me)))
    (push ov isearch-lazy-highlight-overlays)
    ;; 1000 is higher than ediff's 100+,
    ;; but lower than isearch main overlay's 1001
    (overlay-put ov 'priority 1000)
    (overlay-put ov 'face 'lazy-highlight)
    (unless (or (eq isearch-lazy-highlight 'all-windows)
                isearch-lazy-highlight-buffer)
      (overlay-put ov 'window (selected-window)))))

(defun swiper--async-update-input-ivy-scroll-hook (window new-window-start)
  (when (and (eq window (ivy--get-window ivy-last)) (active-minibuffer-window))
    (with-selected-window window
      (swiper--async-mark-candidates-in-range
       new-window-start (window-end (selected-window) t)))))

(defun swiper--async-update-input-ivy-size-hook (frame)
  ; (message "size %S" frame)
  (swiper--async-mark-candidates-in-window))

(defun swiper--async-update-input-ivy ()
  "Called when `ivy' input is updated."
 (with-ivy-window
    (when (and (/= (length ivy-text) 0)
               (> (length (ivy-state-current ivy-last)) 0))
      (let (
            (item (ivy-state-current ivy-last))
            )
        (unless (memq this-command '(ivy-yank-word
                                     ivy-yank-symbol
                                     ivy-yank-char
                                     scroll-other-window
                                     swiper-async))
          (let (
                (begin (swiper--get-begin item))
                )
            (let (
                  (has-match (save-excursion
                               (goto-char begin)
                               (let (
                                     (ending (line-end-position))
                                     )
                                 (if (< begin ending)
                                     (word-search-forward-lax
                                      ivy-text
                                      ending
                                      'on-error-go-to-limit)
                                   nil))))
                  )
              (if has-match
                (let (
                      (mb (match-beginning 0))
                      (me (match-end 0))
                      )
                  (let (
                        (should-scroll (isearch-string-out-of-window me))
                        )
                    (when should-scroll
                      (isearch-back-into-window (eq should-scroll 'above) me)))
                  (isearch-highlight mb me))
                (isearch-dehighlight))))
          (swiper--async-mark-candidates-in-window))))))

(defun swiper--async-ivy (&optional initial-input)
  "Select one of CANDIDATES and move there.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (swiper--init)
  (setq swiper-invocation-face
        (plist-get (text-properties-at (point)) 'face))
  (let ((preselect
         (if swiper-use-visual-line
             (count-screen-lines
              (point-min)
              (save-excursion (beginning-of-visual-line) (point)))
           (1- (line-number-at-pos))))
        (minibuffer-allow-text-properties t)
        res)
    (setq ivy--old-cands nil)
    (setq ivy--all-candidates nil)
    (setq ivy--orig-cands nil)
    (setq ivy--index 0)
    (setq swiper-use-visual-line nil)
    (add-hook 'after-change-functions #'swiper-async-after-change t t)
    (add-hook 'window-scroll-functions #'swiper--async-update-input-ivy-scroll-hook t t)
    (add-hook 'window-size-change-functions #'swiper--async-update-input-ivy-size-hook t t)
    ; (add-hook 'window-configuration-change-hook #'swiper--async-update-input-ivy-hook t t)
    (unwind-protect
         (and
          (setq res
                (ivy-read
                 "Swiper: "
                 'swiper-async-function
                 :dynamic-collection t
                 :initial-input initial-input
                 :keymap swiper-map
                 :preselect preselect
                 :require-match t
                 :update-fn #'swiper--async-update-input-ivy
                 :unwind #'swiper-async--cleanup
                 :action #'swiper--action
                 :re-builder #'swiper--re-builder
                 :history 'swiper-history
                 :caller 'swiper))
          (point))
      (unless (or res swiper-stay-on-quit)
        (goto-char swiper--opoint))
      (unless (or res (string= ivy-text ""))
        (cl-pushnew ivy-text swiper-history))
      (when swiper--reveal-mode
        (reveal-mode 1)))))


(defun ivy-previous-line-hook(orig-fun &rest args)
  (setq swiper--async-direction-backward t)
  (apply orig-fun args))
(advice-add 'ivy-previous-line :around #'ivy-previous-line-hook)
(defun ivy-next-line-hook(orig-fun &rest args)
  (setq swiper--async-direction-backward nil)
  (apply orig-fun args))
(advice-add 'ivy-next-line :around #'ivy-next-line-hook)

(provide 'swiper-async)
