;; -*- lexical-binding: t; -*-
(require 'swiper)
(require 'ivy)
(require 'multiple-cursors-swiper) ; for candidates with advancer and initiater

(setq swiper--async-last-line nil)
(setq swiper--async-last-line-pos nil)
(defun swiper--async-line-at-pos (pos)
  (let (
        (line-no (if (or (null swiper--async-last-line)
                         (null swiper--async-last-line-pos))
                     (progn (goto-char pos) (line-number-at-pos))
                   (let (
                         (lines-diff
                          (- (count-lines
                              pos
                              swiper--async-last-line-pos) 1))
                         )
                     (if (> pos swiper--async-last-line-pos)
                         (+ swiper--async-last-line lines-diff)
                       (- swiper--async-last-line lines-diff)))))
        )
    (setq swiper--async-last-line-pos pos)
    (setq swiper--async-last-line line-no))) ; also, return the line

(defun swiper-line-transformer (str)
  (save-excursion
    (save-restriction
      (widen)
      (let (
            (pos (swiper--get-begin str))
            )
        (concat (format swiper--format-spec
                        (if pos
                            (swiper--async-line-at-pos pos)
                          (swiper--get-line str)))
                (buffer-substring (swiper--get-line-begin str)
                                  (swiper--get-line-end str)))))))

(ivy-set-display-transformer 'swiper-async 'swiper-line-transformer)


(defun swiper--async-action(x)
  (let (
        (beg (swiper--get-begin x))
        (pos (swiper--get-end x))
        )
    (when beg
      (let (
            (has-match (progn (goto-char beg)
                              (if (< beg pos)
                                  (word-search-forward-lax
                                   ivy-text
                                   (line-end-position)
                                   'on-error-go-to-limit)
                                nil)))
            )
        (when has-match
          (setq beg (match-beginning 0))
          (setq pos (match-end 0)))
        (ivy--pulse-region beg pos)
        (goto-char pos)))))


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



(defvar ivy--orig-cands nil
  "Store the original candidates found.")

(defvar ivy--last-cand nil
  "Store the last inserted candidate for faster insertion sort.")
(defvar ivy--last-cand-index 0
  "Store the last inserted candidate index so new candidates will have the correct index.")
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
          (change-index 0)
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
                (when (string-match-p ivy-text item)
                  (cl-incf deleted-matches))
                (setq last-item iterator))
              (when (< (swiper--get-line-end item) change-begin)
                (when (string-match-p ivy-text item)
                  (cl-incf change-index)
                  (cl-decf deleted-matches))
                (setq first-item iterator)))
            (setq iterator (cdr iterator))))
        (setq last-item (if (null last-item) ivy--orig-cands (cdr last-item)))
        (let (
              (search-start (progn (goto-char change-begin) (line-beginning-position)))
              (search-end (progn (goto-char change-end) (line-end-position)))
              (new-candidates)
              (new-candidates-tail)
              )
          (swiper--async-format-spec)
          (swiper--async-iterate-matches
           ivy-text search-start search-end
           (lambda (b e)
             (let (
                   (new-item (list (swiper--async-create-candidate b e)))
                   )
               (when (string-match-p ivy-text (car new-item))
                 (cl-incf found-matches))
               (if (null new-candidates)
                   (progn
                     (setq new-candidates-tail new-item)
                     (setq new-candidates new-item))
                 (setcdr new-candidates-tail new-item)
                 (setq new-candidates-tail new-item)))))
          (let (
                (tail-items (if (null new-candidates)
                                last-item
                              (setcdr new-candidates-tail last-item)
                              new-candidates))
                )
            (if (null first-item)
                (setq ivy--orig-cands tail-items)
              (setcdr first-item tail-items)))))
      (when (>= ivy--index change-index)
        (if (>= deleted-matches (- ivy--index change-index))
            (setq ivy--index change-index)
          (setq ivy--index (- ivy--index deleted-matches)))
        (setq ivy--index (+ ivy--index found-matches)))
        (swiper--async-update-output))
    (setq counsel--async-time (current-time))))

(defun swiper-async-after-change(begin end deleted-length)
  (save-excursion
    (swiper--async-filter (current-buffer) begin end deleted-length)))

(defun swiper-async-after-change-prop(begin end)
  (save-excursion
    (swiper--async-filter (current-buffer) begin end 0)))

(setq to-search nil)
(setq isearch-swiper-limit 3)
(defun swiper-async-function (string)
  "Grep in the current directory for STRING."
  ;; (counsel--elisp-to-pcre (setq ivy--old-re (ivy--regex string)))
  (with-ivy-window
    (setq ivy--old-re nil)
    (setq isearch-string ivy-text)
    (cond
     ((= (length ivy-text) 0)
      (setq to-search nil)
      (setq ivy--old-cands nil)
      (setq ivy--all-candidates nil)
      (setq ivy--orig-cands nil)
      (setq ivy--last-cand nil)
      (setq swiper--async-last-line nil)
      (setq swiper--async-last-line-pos nil)
      (setq ivy--index 0)
      (when (not (null swiper--async-timer))
        (cancel-timer swiper--async-timer)
        (setq swiper--async-timer nil))
      (lazy-highlight-cleanup t)
      (isearch-dehighlight))
     ((or (<= (length ivy-text) isearch-swiper-limit)
          (= (length to-search) 0)
          (not (string-prefix-p to-search ivy-text)))
      (setq to-search (if (< (length ivy-text) isearch-swiper-limit)
                          ivy-text
                        (substring ivy-text 0 isearch-swiper-limit)))
      ;; (= (length ivy--orig-cands) 0)
      (setq ivy--old-cands nil)
      (setq ivy--all-candidates nil)
      (setq ivy--orig-cands nil)
      (setq ivy--last-cand nil)
      (setq swiper--async-last-line nil)
      (setq swiper--async-last-line-pos nil)
      (setq ivy--index 0)
      (swiper--async-init)))
    (setq ivy--all-candidates (if (< (length ivy-text) isearch-swiper-limit)
                                  ivy--orig-cands
                                (ivy--re-filter ivy-text ivy--orig-cands)))))

(defun swiper-async--cleanup ()
  (with-ivy-window
    (setq ivy--old-cands nil)
    (setq ivy--all-candidates nil)
    (setq ivy--orig-cands nil)
    (setq ivy--last-cand nil)
    (setq ivy--index 0)
    (setq to-search nil)
    (setq swiper--async-last-line nil)
    (setq swiper--async-last-line-pos nil)
    (when (not (null swiper--async-timer))
      (cancel-timer swiper--async-timer)
      (setq swiper--async-timer nil))
    (lazy-highlight-cleanup t)
    (isearch-dehighlight)
    (remove-hook 'after-change-functions #'swiper-async-after-change t)
    (remove-hook 'modification-hooks #'swiper-async-after-change-prop t)
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
(setq swiper--async-default-max-matches-per-search 100)
(setq swiper--max-search-length (* 10 4096)) ; one page?

(defun swiper--async-update-output ()
  (ivy--set-candidates ivy--orig-cands)
  (let (
        (this-command 'swiper-async)
        )
    (setq ivy--old-re nil) ; force recalculation
    (ivy--insert-minibuffer
     (ivy--format
      (setq ivy--all-candidates (if (< (length ivy-text) isearch-swiper-limit)
                                    ivy--orig-cands
                                  (ivy--re-filter ivy-text ivy--orig-cands)))))
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
                                         swiper--max-search-length
                                         1))
                                 'on-error-go-to-limit))
                      (cl-incf matches-found)
                      (funcall func (match-beginning 0) (match-end 0)))
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
                    (funcall func (match-beginning 0) (match-end 0)))
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
                    (funcall func (match-beginning 0) (match-end 0)))
                  (setq swiper--async-high-end-point (point))))))))
      (when (/= matches-found 0)
        (swiper--async-update-output)))
    (when (or (< swiper--async-high-start-point
                 swiper--async-high-end-point)
              (< swiper--async-low-start-point
                 swiper--async-low-end-point))
      (schedule-isearch buffer func))))

(defun swiper--async-insertion-sort (candidate-cons comp-func insertion-point)
  (let (
        (idx 0)
        (candidate (car candidate-cons))
        )
    (unless (null insertion-point)
      (unless (funcall comp-func candidate (car insertion-point))
        (cl-incf idx)
        (while (and
                (not (null (cdr insertion-point)))
                (not (funcall comp-func candidate (cadr insertion-point))))
          (cl-incf idx)
          (setq insertion-point (cdr insertion-point)))
        (let (
              (current-cdr (cdr insertion-point))
              )
          (setcdr insertion-point candidate-cons)
          (setcdr candidate-cons current-cdr))))
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

(defun swiper--async-create-candidate (b e)
  (save-restriction
    (widen)
    (let (
          (lb (save-excursion (goto-char b) (point-at-bol)))
          (le (save-excursion (goto-char e) (point-at-eol)))
          (swiper-include-line-number-in-search nil)
          )
      (swiper--fill-candidate-properties
       (buffer-substring-no-properties lb le)
       ; (if (>= (length ivy-text) isearch-swiper-limit)
       ;     (buffer-substring-no-properties lb le)
       ;   " ")
       nil
       0
       t b e lb le))))

(defvar swiper--async-follow-filter-index t)
(defun swiper--async-found-new-candidate (b e)
  (let (
        (candidate (swiper--async-create-candidate b e))
        )
    (let (
          (candidate-cons (list candidate))
          )
      (let (
            (idx (swiper--async-insertion-sort
                  candidate-cons 'candidate--compare ivy--last-cand))
            )
        (if (= idx 0)
            (progn
              (setq idx (swiper--async-insertion-sort
                         candidate-cons 'candidate--compare ivy--orig-cands))
              (if (= idx 0)
                  (if (null ivy--orig-cands)
                      (progn
                        (setq ivy--orig-cands candidate-cons)
                        (setq ivy--last-cand ivy--orig-cands)
                        )
                    (if (candidate--compare candidate (car ivy--orig-cands))
                        (progn
                          (push candidate ivy--orig-cands)
                          (setq ivy--last-cand ivy--orig-cands)
                          )
                      (message "error, must be 0")))
                (setq ivy--last-cand candidate-cons))
              (run-at-time 0 nil 'swiper--async-update-input-ivy))
          (setq ivy--last-cand candidate-cons)
          (when swiper--async-follow-filter-index
            (setq idx (+ ivy--last-cand-index idx))))
        (when swiper--async-follow-filter-index
          (setq ivy--last-cand-index idx))
        (when (string-match-p ivy-text candidate)
            (when (and (>= ivy--index idx)
                       (> (length ivy--orig-cands) 1))
              (cl-incf ivy--index)))))))

(defun swiper--async-format-spec ()
  (let* ((n-lines (count-lines (point-min) (point-max))))
    (let (
          (width (1+ (floor (log n-lines 10))))
          )
      (when (or (null swiper--width) (/= swiper--width width))
        (setq swiper--width width)
        (setq swiper--format-spec (format "%%-%dd: " swiper--width))))))


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
    (swiper--async-format-spec)
    (schedule-isearch
     (current-buffer)
     (lambda (b e)
       (swiper--async-found-new-candidate b e)))))


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
  (swiper--async-mark-candidates-in-window))

(defun swiper--async-update-input-ivy ()
  "Called when `ivy' input is updated."
  (with-ivy-window
    (when (and (/= (length ivy-text) 0)
               (> (length (ivy-state-current ivy-last)) 0))
      (let (
            (item (ivy-state-current ivy-last))
            )
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
                  (when (not (memq this-command '(ivy-yank-word
                                                  ivy-yank-symbol
                                                  ivy-yank-char
                                                  scroll-other-window
                                                  swiper-async)))
                    ; (let (
                    ;       (should-scroll (isearch-string-out-of-window me))
                    ;       )
                    ;   (when should-scroll
                                        ;     (isearch-back-into-window (eq should-scroll 'above) me)))
                    (goto-char me)
                    )
                  (isearch-highlight mb me))
                (isearch-dehighlight))))
          (swiper--async-mark-candidates-in-window)))
    (if which-function-mode
        (which-func-update-1 (selected-window)))))

(defun swiper--async-re-builder(str)
  (word-search-regexp str t))

(defun swiper--async-ivy (&optional initial-input)
  "Select one of CANDIDATES and move there.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (swiper--init)
  (setq swiper-invocation-face
        (plist-get (text-properties-at (point)) 'face))
  (let ((preselect nil)
; (if swiper-use-visual-line
;              (count-screen-lines
;               (point-min)
;               (save-excursion (beginning-of-visual-line) (point)))
;            (1- (line-number-at-pos)))
        (minibuffer-allow-text-properties t)
        res)
    (setq ivy--old-cands nil)
    (setq ivy--all-candidates nil)
    (setq ivy--orig-cands nil)
    (setq ivy--last-cand nil)
    (setq ivy--index 0)
    (setq swiper--async-last-line nil)
    (setq swiper--async-last-line-pos nil)
    (setq swiper-use-visual-line nil)
    (add-hook 'after-change-functions #'swiper-async-after-change t t)
    (add-hook 'modification-hooks #'swiper-async-after-change-prop t t)
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
                 :action #'swiper--async-action
                 :re-builder #'swiper--async-re-builder
                 :history 'swiper-history
                 :sort nil
                 :caller 'swiper-async))
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
