;; -*- lexical-binding: t; -*-
(require 'swiper)
(require 'ivy)
(require 'multiple-cursors-swiper) ; for candidates with advancer and initiater

(setq swiper-include-line-number-in-search nil)
(defun swiper-line-transformer (str)
  (let (
        (n-lines (length ivy--all-candidates)) ; ivy--orig-cands
        )
    (let (
          (swiper--width (1+ (floor (log n-lines 10))))
          )
      (let (
            (swiper--format-spec (format "%%-%dd: " swiper--width))
            )
        (concat (format swiper--format-spec (swiper--get-line str)) str)))))
(ivy-set-display-transformer 'swiper 'swiper-line-transformer)


(defun swiper--async-candidates (addition-begin addition-end)
  (let (
        (swiper-async-initiater (lambda ()
                                  (deactivate-mark)
                                  (goto-char addition-begin)
                                  (beginning-of-line)))
        (swiper-async-advancer (if swiper-use-visual-line
                                   (lambda (n)
                                     (if (>= (line-end-position) addition-end)
                                         (goto-char (point-max))
                                       (line-move n t)))
                                   (lambda (n)
                                     (if (>= (line-end-position) addition-end)
                                         (goto-char (point-max))
                                       (forward-line n)))))
        )
    (swiper--candidates nil
                        :advancer1 swiper-async-advancer
                        :initiater1 swiper-async-initiater
                        :use-format-mode-line nil
                        :include-empty-last-line t)))

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
    (let (
          (str (concat " " (buffer-substring beg end)))
          )
      (put-text-property 0 1 'begin beg str)
      (put-text-property 0 1 'end end str)
      str)))
(defalias 'swiper--line 'swiper--line-with-borders)

(defvar ivy--orig-cands nil
  "Store the original candidates found.")

(defun swiper--async-filter (buffer begin end deleted-length)
  "Receive from buffer the output STR.
Update the minibuffer with the amount of lines collected every
`swiper-async-filter-update-time' microseconds since the last update."
  (when (and t
             ;; (time-less-p (list 0 0 swiper-async-filter-update-time)
             ;; (time-since counsel--async-time))
             )
    (let (
          (working-candidates ivy--orig-cands) ; work on unfiltered candidates
          (original-candidates-length (length ivy--orig-cands))
          )
      (let (
            (chars-diff (- (- end begin) deleted-length))
            (new-candidates (with-current-buffer buffer (swiper--async-candidates begin end)))
            )
        (let (
              (new-candidates-length (length new-candidates))
              (cands)
              (first-new-candidate (first new-candidates))
              (last-new-candidate (car (last new-candidates)))
              (i 0)
              (first-index)
              (last-index)
              (deleted-lines 0)
              (added-lines 0)
              (deleted-leftover deleted-length)
              (lines-diff 0)
              )
          (cl-loop for cand in working-candidates do
                   (when (> new-candidates-length 0)
                     (when (= (swiper--get-line cand)
                              (swiper--get-line first-new-candidate))
                       (setq first-index i))
                     (when (= (swiper--get-line cand)
                              (swiper--get-line last-new-candidate))
                       (setq last-index i)))
                   (let (
                         (cand-begin (swiper--get-begin cand))
                         (cand-end (swiper--get-end cand))
                         )
                     (when (and (not (null first-index))
                                (> deleted-leftover 0))
                       (let (
                             (begin-reference (if (and (>= begin cand-begin)
                                                       (<= begin cand-end))
                                                  begin
                                                cand-begin))
                             )
                         (let (
                               (left-in-line (+ (- cand-end begin-reference) 1))
                               )
                           (let (
                                 (drained-length (min deleted-leftover
                                                      left-in-line))
                                 )
                             (when (= left-in-line drained-length)
                               (cl-incf deleted-lines))
                             (setq deleted-leftover (- deleted-leftover
                                                       drained-length)))))))
                   (cl-incf i))
          (when (null first-index)
            (setq first-index original-candidates-length))
          (when (null last-index)
            (setq last-index original-candidates-length))
          (setq added-lines new-candidates-length)
          (setq deleted-lines (+ deleted-lines 1))
          (unless (= deleted-leftover 0)
            (warn "deleted-leftover (=%S) != 0" deleted-leftover))
          (unless (= added-lines (+ (- last-index first-index) 1))
            (warn "amount of added cands %S != diff of first %S and last %S lines = %S" new-candidates-length last-index first-index added-lines))
          (let (
                (cand-lasts (nthcdr (+ first-index deleted-lines) working-candidates))
                (i (+ first-index added-lines 1))
                )
            (let (
                  (n-lines (+ first-index added-lines (length cand-lasts)))
                  )
              (let (
                    (numbers-width nil) ; &optional numbers-width
                    )
                (setq swiper--width (or numbers-width (1+ (floor (log n-lines 10)))))
                (setq swiper--format-spec (format "%%-%dd " swiper--width))
                (let (
                      (new-cand-lasts)
                      )
                  (dolist (cand cand-lasts new-cand-lasts)
                    (push (swiper--fill-candidate-properties
                           (if swiper-include-line-number-in-search
                               (string-remove-prefix (swiper--get-str-line cand) cand)
                             cand)
                           swiper--format-spec i nil) new-cand-lasts)
                    (cl-incf i))
                  (setq new-cand-lasts (nreverse new-cand-lasts))
                  (if (= new-candidates-length 0)
                      (setq new-candidates new-cand-lasts)
                    (setcdr (last new-candidates) new-cand-lasts))))))
          (if (> first-index 0)
              (setcdr (nthcdr (- first-index 1) working-candidates)
                      new-candidates)
            (setq working-candidates new-candidates))
          (setq ivy--index (+ ivy--index lines-diff))))
      (ivy--set-candidates working-candidates))
    (setq ivy--orig-cands ivy--all-candidates)
    (let (
          (this-command 'swiper-async)
          )
      (setq ivy--old-re nil) ; force recalculation
      (ivy--insert-minibuffer
         (ivy--format
          (ivy--filter ivy-text ivy--all-candidates))))
    (setq counsel--async-time (current-time))))

(defun swiper-async-after-change(begin end deleted-length)
  (swiper--async-filter (current-buffer) begin end deleted-length))

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
    (lazy-highlight-cleanup t)
    (isearch-dehighlight)
    (remove-hook 'after-change-functions #'swiper-async-after-change t))
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
      (ivy--filter ivy-text ivy--all-candidates)))))

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
            (when (not swiper--async-direction-backward)
              (when (< swiper--async-high-start-point
                       swiper--async-high-end-point)
                (goto-char swiper--async-high-start-point)
                (while (and (not (input-pending-p))
                            (< matches-found swiper--async-max-matches-per-search)
                            (word-search-forward-lax
                             to-search
                             (min swiper--async-high-end-point
                                  (+ swiper--async-high-start-point
                                     swiper--max-search-length))
                             'on-error-go-to-limit))
                  (cl-incf matches-found)
                  (funcall func (match-beginning 0) (match-end 0)))
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
                                     swiper--max-search-length))
                             'on-error-go-to-limit))
                  (cl-incf matches-found)
                  (funcall func (match-beginning 0) (match-end 0)))
                (setq swiper--async-low-start-point (point)))))))
      (when (/= matches-found 0)
        (swiper--async-update-output)))
    (when (and (< swiper--async-high-start-point
                  swiper--async-high-end-point)
               (< swiper--async-low-start-point
                  swiper--async-low-end-point))
      (schedule-isearch buffer func))))

(defun swiper--async-insertion-sort (candidate comp-func)
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
          (setq insertion-point (cdr insertion-point)))
        (let (
              (current-cdr (cdr insertion-point))
              (current-candidate (list candidate))
              )
          (setcdr insertion-point current-candidate)
          (setcdr current-candidate current-cdr))))))

(defun swiper--async-init ()
  (setq counsel--async-time (current-time))
  (setq counsel--async-start counsel--async-time)
  (with-ivy-window
    (if (not swiper--async-direction-backward)
        (setq swiper--async-high-start-point (window-start))
      (setq swiper--async-high-start-point (window-end)))
    (setq swiper--async-high-end-point (point-max))
    (setq swiper--async-low-start-point (point-min))
    (setq swiper--async-low-end-point swiper--async-high-start-point)
    (let* ((n-lines (count-lines (point-min) (point-max))))
      (unless (zerop n-lines)
        (setq swiper--width (1+ (floor (log n-lines 10))))
        (setq swiper--format-spec (format "%%-%dd " swiper--width))
        (when (not (null swiper--async-timer))
          (cancel-timer swiper--async-timer)
          (setq swiper--async-timer nil))
        (let (
              (buffer (current-buffer))
              )
          (schedule-isearch
           buffer
           (lambda (b e)
             (swiper--async-insertion-sort
              (swiper--fill-candidate-properties
               (buffer-substring
                (save-excursion (goto-char b) (line-beginning-position))
                (save-excursion (goto-char e) (line-end-position)))
               swiper--format-spec
               (line-number-at-pos)
               t b e)
              (lambda (c1 c2)
                (let (
                      (l1 (swiper--get-line c1))
                      (l2 (swiper--get-line c2))
                      )
                  (or (< l1 l2)
                      (and (= l1 l2)
                           (< (swiper--get-end c1)
                              (swiper--get-end c2))))))))))))))


(defun swiper--async-mark-candidates-in-window ()
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
    swiper--point-max)))

(defun swiper--async-mark-candidates-in-range (beg end)
  ;; (save-excursion (swiper--add-overlays ivy-text beg end))
  ;; (save-excursion (isearch-lazy-highlight-new-loop beg end))
  (save-excursion
    (lazy-highlight-cleanup t)
    (goto-char beg)
    (while (word-search-forward-lax
            ivy-text
            end
            'on-error-go-to-limit)
      (swiper--async-mark-candidate (match-beginning 0) (match-end 0)))))

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

(defun swiper--async-update-input-ivy ()
  "Called when `ivy' input is updated."
 (with-ivy-window
    (swiper--cleanup)
    (when (> (length (ivy-state-current ivy-last)) 0)
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
                               (word-search-forward-lax
                                ivy-text
                                (line-end-position)
                                'on-error-go-to-limit)))
                  )
              (if has-match
                (let (
                      (mb (match-beginning 0))
                      (me (match-end 0))
                      )
                  (isearch-back-into-window swiper--async-direction-backward me)
                  (isearch-highlight mb me))
                (isearch-dehighlight)))
          (swiper--async-mark-candidates-in-window)))))))

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
    (setq swiper-use-visual-line nil)
    (add-hook 'after-change-functions #'swiper-async-after-change t t)
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

(provide 'swiper-async)
