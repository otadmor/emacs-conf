;; -*- lexical-binding: t; -*-
(require 'swiper)
(require 'ivy)
(require 'multiple-cursors-swiper) ; for candidates with advancer and initiater

(setq swiper--async-last-line nil)
(setq swiper--async-last-line-pos nil)
(defun swiper--async-line-at-pos (pos)
  (let (
        (line-no
         (if (or (null swiper--async-last-line)
                 (null swiper--async-last-line-pos))
             (progn (goto-char pos) (line-number-at-pos))
           (let (
                 (small-pos (min pos swiper--async-last-line-pos))
                 (big-pos (max pos swiper--async-last-line-pos))
                 )
             (let (
                   (big-pos-at-newline
                    (= big-pos (progn (goto-char big-pos)
                                      (line-beginning-position))))
                   (lines-diff (count-lines small-pos big-pos))
                   )
               (unless big-pos-at-newline
                 (setq lines-diff (- lines-diff 1)))
               (if (> pos swiper--async-last-line-pos)
                   (+ swiper--async-last-line lines-diff)
                 (- swiper--async-last-line lines-diff))))))
        )
    (setq swiper--async-last-line-pos pos)
    (setq swiper--async-last-line line-no))) ; also, return the line

(defun swiper-line-transformer (str)
  (save-excursion
    (save-restriction
      (widen)
      (let (
            (pos (swiper--get-begin str))
            (line-beg (if swiper-include-line-number-in-search
                          (swiper--get-line-begin str)
                        nil))
            )
        (let (
              (line-str (if swiper-include-line-number-in-search
                            (format swiper--format-spec
                                    (if pos
                                        ;; XXX : need to execute this with
                                        ;; XXX : something similar
                                        ;; XXX : to with-timeout,
                                        (swiper--async-line-at-pos pos)
                                      (swiper--get-line str)))
                          ""))
              (cand-substr (buffer-substring (swiper--get-line-begin str)
                                             (swiper--get-line-end str)))
              )
          (let (
                (line-str-len (length line-str))
                (res (concat line-str cand-substr))
                (re-str (funcall ivy--regex-function ivy-text))
                )
            (let (
                  (line-match (swiper--async-match re-str str))
                  )
              (when line-match
                (let (
                      (beg (car line-match))
                      (end (cdr line-match))
                      )
                  (let (
                        (highlight-beg (+ line-str-len beg))
                        (highlight-end (+ line-str-len end))
                        )
                    (put-text-property
                     0 1 'region-data
                     (cons
                      (swiper--async-create-marker highlight-end)
                      (swiper--async-create-marker highlight-beg)) res)))))
            res))))))
(ivy-set-display-transformer 'swiper-async 'swiper-line-transformer)

(defun swiper--async-match-in-buffer (item)
  (let (
        (re-str (funcall ivy--regex-function ivy-text))
        )
    (let (
          (beg (swiper--get-begin item))
          (pos (swiper--get-end item))
          (positive-re (swiper-async--join-re-positive re-str))
          ; (negative-re (swiper-async--join-re-negative re-str))
          )
      (when beg
        ;; TODO : loop over untile positive-re found and not matching negative-re
        (let (
              (has-match (progn (goto-char beg)
                                (if (< beg pos)
                                    (re-search-forward
                                     positive-re
                                     (line-end-position)
                                     'on-error-go-to-limit)
                                  nil)))
              )
          (if has-match
              (cons (match-beginning 0) (match-end 0))
            (cons beg pos)))))))

(defun swiper--async-action(x)
  (let (
        (res (swiper--async-match-in-buffer x))
        )
    (let (
          (beg (car res))
          (pos (cdr res))
          )
      (ivy--pulse-region beg pos)
      (goto-char pos))))

(defcustom swiper-async-filter-update-time 50
  "The amount of microseconds to wait until updating `swiper--async-filter'."
  :type 'integer)

(defun swiper--async-create-marker (point)
  (set-marker (make-marker)
              (let ((mark-even-if-inactive t))
                point)))

(defun swiper-async--fill-candidate-properties (str swiper--format-spec line-no &optional begin end line-begin line-end)
  (setq str (ivy-cleanup-string str))
  (put-text-property
   0 1 'swiper-no-line-number line-no str)
  (put-text-property
   0 1 'region-data (cons
                     (swiper--async-create-marker end)
                     (swiper--async-create-marker begin)) str)
  (put-text-property
   0 1 'line-region-data (cons
                          (swiper--async-create-marker line-end)
                          (swiper--async-create-marker line-begin)) str)
  str)

(defun swiper--get-line (item)
  (get-text-property 0 'swiper-no-line-number item))
(defun swiper--get-str-line (item)
  (get-text-property 0 'swiper-line-number item))
(defun swiper--get-region (item)
  (get-text-property 0 'region-data item))
(defun swiper--get-begin (item)
  (cdr (swiper--get-region item)))
(defun swiper--get-end (item)
  (car (swiper--get-region item)))
(defun swiper--get-line-region (item)
  (get-text-property 0 'line-region-data item))
(defun swiper--get-line-begin (item)
  (cdr (swiper--get-line-region item)))
(defun swiper--get-line-end (item)
  (car (swiper--get-line-region item)))



(defvar ivy--orig-cands nil
  "Store the original candidates found.")

(defvar ivy--last-cand nil
  "Store the last inserted candidate for faster insertion sort.")
(defvar ivy--next-cand-index 0
  "Store the last inserted candidate index so new candidates will have the correct index.")

(defun swiper--async-iterate-matches (pattern beg end func)
  (when (and (/= (length pattern) 0) (< beg end))
    (goto-char beg)
    (let (
          (re-str (funcall ivy--regex-function pattern))
          )
      (let (
            (positive-re (swiper-async--join-re-positive re-str))
            ; (negative-re (swiper-async--join-re-negative re-str))
            )
        (while (re-search-forward
                positive-re
                end
                'on-error-go-to-limit)
          (funcall func (match-beginning 0) (match-end 0)))))))

(defun swiper--async-make-startwith-match (re-str)
  (if (string-prefix-p "^" re-str) re-str (concat "^" re-str "")))

(defun swiper--async-match (re item)
  (if (null re)
      nil
    (let (
          (rel-begin (- (swiper--get-begin item)
                        (swiper--get-line-begin item)))
          (rel-end (- (swiper--get-end item)
                      (swiper--get-line-begin item)))
          (line-length (- (swiper--get-line-end item)
                          (swiper--get-line-begin item)))
          )
      (let (
            (str (substring item rel-begin line-length))
            (re-seq (if (listp re) re (list (cons re t))))
            )
        (let (
              (re-seq (mapcar
                       (lambda (x) (cons (swiper--async-make-startwith-match
                                          (car x)) (cdr x)))
                       re-seq))
              )
          ;; ivy-re-match
          (let ((res t)
                re)
            (while (and res (setq re (pop re-seq)))
              (setq res
                    (if (cdr re)
                        (string-match (car re) str)
                      (not (string-match (car re) str)))))
            (when res
              (cons (+ rel-begin (match-beginning 0))
                    (+ rel-begin (match-end 0))))))))))

(defun swiper--async-matchp (re item)
  (not (null (save-match-data (swiper--async-match re item)))))

(defun swiper--async-filter (buffer change-begin inserted-end deleted-length)
  "Receive from buffer the output STR.
Update the minibuffer with the amount of lines collected every
`swiper-async-filter-update-time' microseconds since the last update."
  (let (
        (re-str (funcall ivy--regex-function ivy-text))
        )
    (when (and (not (null swiper--async-last-line-pos))
               (< change-begin swiper--async-last-line-pos))
      (setq swiper--async-last-line nil)
      (setq swiper--async-last-line-pos nil))
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
                (when (swiper--async-matchp re-str item)
                  (cl-incf deleted-matches))
                (setq last-item iterator))
              (when (< (swiper--get-line-end item) change-begin)
                (when (swiper--async-matchp re-str item)
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
           to-search search-start search-end
           (lambda (b e)
             (let (
                   (new-item (list (swiper--async-create-candidate b e)))
                   )
               (when (swiper--async-matchp re-str (car new-item))
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
     ((= (length ivy-text) 0) (swiper--async-reset-state))
     ((or (<= (length ivy-text) isearch-swiper-limit)
          (= (length to-search) 0)
          (not (string-prefix-p to-search ivy-text)))
      (swiper--async-reset-state)
      (setq to-search (if (< (length ivy-text) isearch-swiper-limit)
                          ivy-text
                        (substring ivy-text 0 isearch-swiper-limit)))
      (swiper--async-init)))
    (swiper--async-update-all-candidates t)))

(defun swiper--async-update-all-candidates(&optional follow-ivy-index)
  (setq ivy--all-candidates
        (let (
              (re-list (funcall ivy--regex-function ivy-text))
              )
          (let (
                (idx nil)
                (first-past-opoint-idx nil)
                )
            (let (
                  (filtered-results
                   (ivy--re-filter re-list
                                   ivy--orig-cands
                                   (lambda (re-str)
                                     (lambda (x)
                                       (let (
                                             (has-match
                                              (swiper--async-matchp re-str x))
                                             )
                                         ;; TODO this is problematic if
                                         ;; the re is negated.
                                         ;; the condition should be:
                                         ;; (when (xor has-match negated) ...)
                                         (when has-match
                                           (if (null idx)
                                               (setq idx 0)
                                             (cl-incf idx))
                                           (when (and (>= (swiper--get-begin x)
                                                          swiper--opoint)
                                                      (null first-past-opoint-idx))
                                             (setq first-past-opoint-idx idx)))
                                         has-match)))))
                  )
              (when (and follow-ivy-index
                         (not (null first-past-opoint-idx)))
                (setq ivy--index first-past-opoint-idx))
              filtered-results)))))

(defun swiper-async--cleanup ()
  (with-ivy-window
    (swiper--async-reset-state)
    ;; (remove-hook 'window-configuration-change-hook #'swiper--async-update-input-ivy-hook t)
    (remove-hook 'after-change-functions #'swiper-async-after-change t)
    (remove-hook 'modification-hooks #'swiper-async-after-change-prop t)
    (remove-hook 'window-scroll-functions #'swiper--async-update-input-ivy-scroll-hook t)
    (remove-hook 'window-size-change-functions #'swiper--async-update-input-ivy-size-hook t)))


(defun swiper--async-swiper--cleanup-hook ()
  (with-ivy-window
    (lazy-highlight-cleanup t)
    (isearch-dehighlight)))
(advice-add 'swiper--cleanup :after #'swiper--async-swiper--cleanup-hook)

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
      (swiper--async-update-all-candidates)))
    (swiper--async-update-input-ivy)))

(defun swiper-async--join-re-positive (re-str)
  (if (string-or-null-p re-str)
      re-str
    (let (
          (final-re "")
          )
      (let* ((regexp-or-regexps re-str)
             (regexps
              (if (listp regexp-or-regexps)
                  (mapcar #'car (cl-remove-if-not #'cdr regexp-or-regexps))
                (list regexp-or-regexps))))
        (dolist (re regexps)
          (let (
                (sep (if (= (length final-re) 0) "" "|"))
                )
            (setq final-re (concat final-re sep re)))))
      final-re)))

(defun swiper--async-isearch(buffer func)
  (when (and (/= (length to-search) 0)
             (active-minibuffer-window)
             )
    (let (
          (re-str (funcall ivy--regex-function to-search))
          )
      (let (
            (matches-found 0)
            (positive-re (swiper-async--join-re-positive re-str))
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
                                  (< matches-found
                                     swiper--async-max-matches-per-search)
                                  (re-search-forward
                                   positive-re
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
                                  (< matches-found
                                     swiper--async-max-matches-per-search)
                                  (re-search-forward
                                   positive-re
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
                                (< matches-found
                                   swiper--async-max-matches-per-search)
                                (re-search-backward
                                 positive-re
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
                                (< matches-found
                                   swiper--async-max-matches-per-search)
                                (re-search-backward
                                 positive-re
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
        (schedule-isearch buffer func)))))

(defun swiper--async-insertion-sort (candidate-cons comp-func insertion-point)
  (let (
        (idx nil)
        (candidate (car candidate-cons))
        (re-str (funcall ivy--regex-function ivy-text))
        )
    (unless (null insertion-point)
      (when (not (funcall comp-func candidate (car insertion-point)))
        (setq idx 0)
        (while (and
                (not (null (cdr insertion-point)))
                (not (funcall comp-func candidate (cadr insertion-point))))
          (setq insertion-point (cdr insertion-point))
          (when (swiper--async-matchp re-str (car insertion-point))
            (cl-incf idx)))
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
          )
      (swiper-async--fill-candidate-properties
       (buffer-substring-no-properties lb le)
       nil
       0
       b e lb le))))

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
        (when (null idx)
          (setq ivy--next-cand-index 0)
          (setq idx (swiper--async-insertion-sort
                     candidate-cons 'candidate--compare ivy--orig-cands))
          (when (null idx)
            (setq idx 0)
            (if (null ivy--orig-cands)
                (progn
                  (setq ivy--orig-cands candidate-cons)
                  (run-at-time 0 nil 'swiper--async-update-input-ivy)
                  )
              (if (candidate--compare candidate (car ivy--orig-cands))
                  (progn
                    (setcdr candidate-cons ivy--orig-cands)
                    (setq ivy--orig-cands candidate-cons)
                    )
                (message "error, must be 0")))))
        (setq ivy--last-cand candidate-cons)
        (let (
              (re-str (funcall ivy--regex-function ivy-text))
              )
          (when (swiper--async-matchp re-str candidate)
            (setq idx (+ ivy--next-cand-index idx))
            (setq ivy--next-cand-index (+ idx 1))
            (when (and (>= ivy--index idx)
                       (> (length ivy--orig-cands) 1))
              (cl-incf ivy--index))))))))

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
     'swiper--async-found-new-candidate)))


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

(defcustom ivy-marker-functions-alist
  '((ivy--regex-plus . swiper--async-mark-candidates-in-range-ivy)
    (ivy--regex-ignore-order . swiper--async-mark-candidates-in-range-ivy)
    (ivy--regex-fuzzy . swiper--async-mark-candidates-in-range-ivy)
    (regexp-quote . swiper--async-mark-candidates-in-range-isearch)
    (swiper--regexp-builder . swiper--async-mark-candidates-in-range-isearch)
    )
  "Alist of preferred markers with the marker function."
  :type '(alist :key-type function :value-type string))

(defun swiper--async-mark-candidates-in-range (beg end)
  (setq ivy--marker-function
        (or (cdr (assq ivy--regex-function ivy-marker-functions-alist))
            #'swiper--async-mark-candidates-in-range-ivy))
  (funcall ivy--marker-function beg end))

(defun swiper--async-mark-candidates-in-range-ivy (beg end)
  (when (> (length (ivy-state-current ivy-last)) 0)
    (let (
          (re-str (funcall ivy--regex-function ivy-text))
          )
      (let* ((regexp-or-regexps re-str)
             (regexps
              (if (listp regexp-or-regexps)
                  (mapcar #'car (cl-remove-if-not #'cdr regexp-or-regexps))
                (list regexp-or-regexps))))
        (dolist (re regexps)
          (let* ((re (replace-regexp-in-string
                      "    " "\t"
                      re)))
            (swiper--add-overlays re beg end)))))))

(defun swiper--async-mark-candidates-in-range-isearch (beg end)
  ;; (save-excursion (swiper--add-overlays ivy-text beg end))
  ;; (save-excursion (isearch-lazy-highlight-new-loop beg end))
  (save-excursion
    (lazy-highlight-cleanup t)
    (swiper--async-iterate-matches
     ivy-text beg end
     ;; TODO : should we check for negative-re here?
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
	(overlay-put isearch-overlay 'face isearch-face)
        (unless (or (eq isearch-lazy-highlight 'all-windows)
                    isearch-lazy-highlight-buffer)
          (overlay-put isearch-overlay 'window (selected-window))))))

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

(defun swiper--async-highlighter (str)
  (let (
        (beg (swiper--get-begin str))
        (end (swiper--get-end str))
        )
    (unless (or (null beg)
                (null end))
      (ivy-add-face-text-property beg end 'lazy-highlight str)))
  str)
(add-to-list 'ivy-highlight-functions-alist '(swiper--regexp-builder . swiper--async-highlighter))
(add-to-list 'ivy-highlight-functions-alist '(regexp-quote . swiper--async-highlighter))




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
    (swiper--cleanup)
    (when (> (length ivy--all-candidates) 0)
      (let (
            (item (ivy-state-current ivy-last))
            )
        (let (
              (res (swiper--async-match-in-buffer item))
              )
          (let (
                (beg (car res))
                (pos (cdr res))
                )
            (when (not (memq this-command '(ivy-yank-word
                                            ivy-yank-symbol
                                            ivy-yank-char
                                            scroll-other-window
                                            swiper-async)))
              (goto-char pos))
            (let (
                  (search-highlight t)
                  )
              (isearch-highlight beg pos))))))
    (swiper--async-mark-candidates-in-window)))

(defun swiper--async-reset-state ()
  (setq to-search nil)
  (setq ivy--old-cands nil)
  (setq ivy--all-candidates nil)
  (setq ivy--orig-cands nil)
  (setq ivy--last-cand nil)
  (setq ivy--next-cand-index 0)
  (setq swiper--async-last-line nil)
  (setq swiper--async-last-line-pos nil)
  (setq ivy--index 0)
  (setq swiper-use-visual-line nil)
  (when (not (null swiper--async-timer))
    (cancel-timer swiper--async-timer)
    (setq swiper--async-timer nil))
  (swiper--cleanup))

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
    (swiper--async-reset-state)
    (add-hook 'after-change-functions #'swiper-async-after-change t t)
    (add-hook 'modification-hooks #'swiper-async-after-change-prop t t)
    (add-hook 'window-scroll-functions #'swiper--async-update-input-ivy-scroll-hook t t)
    (add-hook 'window-size-change-functions #'swiper--async-update-input-ivy-size-hook t t)
    ; (add-hook 'window-configuration-change-hook #'swiper--async-update-input-ivy-hook t t)
    (unwind-protect
         (and
          (setq res
                (ivy-read
                 "Swiper [%BUILDER]: "
                 'swiper-async-function
                 :dynamic-collection t
                 :initial-input initial-input
                 :keymap swiper-map
                 :preselect preselect
                 :require-match t
                 :update-fn #'swiper--async-update-input-ivy
                 :unwind #'swiper-async--cleanup
                 :action #'swiper--async-action
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

(defun ivy-rotate-preferred-builders-update()
  (swiper-async-function ivy-text)
  (setq ivy--highlight-function
        (or (cdr (assq ivy--regex-function ivy-highlight-functions-alist))
              #'ivy--highlight-default))
  (setq ivy--prompt (ivy-add-prompt-count
                     (ivy--quote-format-string
                      (or (ivy-state-prompt ivy-last) "")))))
(advice-add 'ivy-rotate-preferred-builders :after #'ivy-rotate-preferred-builders-update)

(defun swiper--async-which-func-update ()
  (with-ivy-window
    (if which-function-mode
        (which-func-update-1 (selected-window)))))
(advice-add 'swiper--async-update-input-ivy :after #'swiper--async-which-func-update)

(defun swiper--regexp-builder (x) x)

(add-to-list 'ivy-preferred-re-builders '(regexp-quote . "text"))
(add-to-list 'ivy-preferred-re-builders '(swiper--regexp-builder . "regexp"))



(defun ivy--regex-fuzzy (str)
  "Build a regex sequence from STR.
Insert .* between each char."
  (if (string-match "\\`\\(\\^?\\)\\(.*?\\)\\(\\$?\\)\\'" str)
      (prog1
          (concat (match-string 1 str)
                  (let ((lst (string-to-list (match-string 2 str))))
                    (apply #'concat
                           (cl-mapcar
                            #'concat
                            (cons "" (cdr (mapcar (lambda (c) (format "[^%c\n]*" c))
                                                  lst)))
                            (mapcar (lambda (x) (format "\\(%s\\)" (regexp-quote (char-to-string x))))
                                    lst))))
                  (match-string 3 str))
        (setq ivy--subexps (length (match-string 2 str))))
    str))

(defun ivy--quote-format-string-hook(orig-fun str)
  (funcall orig-fun
           (replace-regexp-in-string
            "%BUILDER"
            (cdr (assq ivy--regex-function ivy-preferred-re-builders))
            str t t)))
(advice-add 'ivy--quote-format-string :around #'ivy--quote-format-string-hook)

(defun ivy-previous-line-hook(orig-fun &rest args)
  (setq swiper--async-direction-backward t)
  (apply orig-fun args))
(advice-add 'ivy-previous-line :around #'ivy-previous-line-hook)
(defun ivy-next-line-hook(orig-fun &rest args)
  (setq swiper--async-direction-backward nil)
  (apply orig-fun args))
(advice-add 'ivy-next-line :around #'ivy-next-line-hook)

(provide 'swiper-async)
