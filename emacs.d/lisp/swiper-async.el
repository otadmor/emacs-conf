;; -*- lexical-binding: t; -*-
(require 'swiper)
(require 'ivy)
(require 'counsel) ; for counsel--async

(defmacro benchmark-and-get-result (&rest forms)
  "Return the time in seconds elapsed for execution of FORMS.
Returns both the time and the result."
  (declare (indent 0) (debug t))
  (let ((t1 (make-symbol "t1"))(res (make-symbol "res")))
    `(let (,t1)
       (setq ,t1 (current-time))
       (let (
             (,res (progn ,@forms))
             )
       (cons (float-time (time-since ,t1)) ,res)))))

(defvar swiper--async-last-line nil
  "Remembers the last line of swiper--async-last-line-pos.")
(defvar swiper--async-last-line-pos nil
  "Saved the last searched for line position.")
(defvar swiper--async-max-line-count-size 4096
  "The size swiper--async-line-at-pos limits its count-lines and
swiper--async-isearch splits the buffer to when filling line numbers.
Changed dynamically according to swiper--async-max-line-count-time and the
actual performance of the count-lines.")
(defcustom swiper--async-max-line-count-time 0.01
  "The soft maximum time swiper--async-line-at-pos is allowed to run."
  :type 'number)

(defun swiper--async-line-at-pos (pos &optional force)
  "Differential count-lines. It saves a previous position and the lines
at that position, and count the remaining lines from the saved position
to the requested position.
FORCE will ignore the limit of swiper--async-max-line-count-time."
  (let (
        (line-no
         (if (or (= (point-min) pos)
                 (null swiper--async-last-line)
                 (null swiper--async-last-line-pos))
             (progn (goto-char pos) (line-number-at-pos))
           (let (
                 (small-pos (min pos swiper--async-last-line-pos))
                 (big-pos (max pos swiper--async-last-line-pos))
                 )
             (when (or
                    (<= (- big-pos small-pos) swiper--async-max-line-count-size)
                    force)
               (let (
                     (big-pos-at-newline
                      (= big-pos (progn (goto-char big-pos)
                                        (line-beginning-position))))
                     (lines-diff-and-time (benchmark-and-get-result
                                           (count-lines small-pos big-pos)))
                     )
                 (let (
                       (lines-time (car lines-diff-and-time))
                       (lines-diff (cdr lines-diff-and-time))
                       )
                   (when (/= big-pos small-pos)
                     (setq swiper--async-max-line-count-size
                           (max
                            (min (ceiling (/ (* (- big-pos small-pos)
                                                swiper--async-max-line-count-time)
                                             lines-time))
                                 1048576) 128)))
                   (unless big-pos-at-newline
                     (setq lines-diff (- lines-diff 1)))
                   (if (> pos swiper--async-last-line-pos)
                       (+ swiper--async-last-line lines-diff)
                     (- swiper--async-last-line lines-diff))))))))
        )
    (unless (null line-no)
      (setq swiper--async-last-line-pos pos)
      (setq swiper--async-last-line line-no)))) ; also, return the line

(defun swiper-line-transformer (str)
  "a transformer to swiper--async. it reads the candidate line from the buffer
(including properties), fills up line number if needed and returns the line
trimmed to the minibuffer size, while keeping the match at the middle of the
minibuffer."
  (save-excursion
    (save-restriction
      (widen)
      (let* (
             (pos (swiper--get-begin str))
             (beg-end (swiper--async-match-in-buffer str))
             (line-beg (swiper--get-line-begin str))
             (line-end (swiper--get-line-end str))
             (line-str (if swiper-include-line-number-in-search
                           (format swiper--format-spec
                                   ;; XXX : need to execute this with
                                   ;; XXX : something similar
                                   ;; XXX : to with-timeout,
                                   (or (swiper--async-line-at-pos pos)
                                       0))
                         ""))
             (beg (if (not (null beg-end)) (car beg-end)
                    (swiper--get-begin str)))
             (end (if (not (null beg-end)) (cdr beg-end)
                    (swiper--get-end str)))
             (line-str-len (length line-str))
             (match-len (- end beg))
             (line-len (- line-end line-beg))
              ; (minibuffer-prompt-width)
             (available-width (max (- (with-selected-window
                                          (active-minibuffer-window)
                                        (window-width)) line-str-len) 0))
             (after-match-len (- line-end end))
             (before-match-len (- beg line-beg))
             (clip-range
              (cond
               ((< line-len available-width) (cons line-beg line-end))
               ((> match-len available-width) (cons pos (+ pos available-width)))
               ((< (+ before-match-len match-len) available-width)
                (cons line-beg (+ line-beg available-width)))
               ((< (+ match-len after-match-len) available-width)
                (cons (- line-end available-width) line-end))
               (t (let (
                        (half-diff (/ (- available-width match-len) 2))
                        )
                    (cons (- beg half-diff) (+ end half-diff))))))
             (clip-beg (car clip-range))
             (clip-end (cdr clip-range))
             (highlight-beg (+ line-str-len (- beg clip-beg)))
             (highlight-end (+ line-str-len (- (min clip-end
                                                    end) clip-beg)))
             (cand-substr (buffer-substring clip-beg clip-end))
             (res (concat line-str cand-substr))
             )
        (put-text-property
         0 1 'region-data
         (cons
          (swiper--async-create-marker highlight-end)
          (swiper--async-create-marker highlight-beg)) res)
        (put-text-property
         0 1 'line-region-data
         (cons
          nil
          line-beg) res)
        res))))
(ivy-set-display-transformer 'swiper-async 'swiper-line-transformer)

(defun swiper--async-match-in-buffer (item)
  "Returns the actual match beginning and end of the candidate.
The saved candidate end position might not be the real end because the matched
candidate is limited to isearch-swiper-limit(default=3) while searching."
  (let (
        (re-str swiper--async-ivy-text-re)
        )
    (let (
          (beg (swiper--get-begin item))
          (pos (swiper--get-end item))
          (positive-re swiper--async-ivy-text-positive-re)
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
  "goto the candidate position in the file and mark it for a second for the
user to see where it is."
  (let (
        (res (let (
                   (swiper--async-ivy-text-re
                    (funcall ivy--regex-function ivy-text))
                   )
               (swiper--async-match-in-buffer x)))
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
  "This function used to create a marker from the given point (which is
integer). for performances reasons we do not save a marker anymore, and handle
movement of matches in the filter function."
  point)

(defun swiper-async--fill-candidate-properties (str swiper--format-spec line-no &optional begin end line-begin line-end)
  "Create the candidate data structure from its properties. The candidate
data structure is actually a propertized string."
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

; (defun swiper--get-line (item)
;   (get-text-property 0 'swiper-no-line-number item))
; (defun swiper--get-str-line (item)
;   (get-text-property 0 'swiper-line-number item))
(defun swiper--get-region (item)
  "Returns a cons with the end and the beginning of the candidate."
  (get-text-property 0 'region-data item))
(defun swiper--get-begin (item)
  "Returns the beginning position of the candidate."
  (cdr (swiper--get-region item)))
(defun swiper--get-end (item)
  "Returns the ending position of the candidate."
  (car (swiper--get-region item)))
(defun swiper--get-line-region (item)
  "Returns a cons with the end and the beginning of the candidate's line."
  (get-text-property 0 'line-region-data item))
(defun swiper--get-line-begin (item)
  "Returns the beginning of the candidate's line."
  (cdr (swiper--get-line-region item)))
(defun swiper--get-line-end (item)
  "Returns the ending of the candidate's line."
  (car (swiper--get-line-region item)))
(defun swiper--async-move (item chars-diff)
  "Moves the candidate's begin, end, line-begin and line-end by position
of chars-diff. This is called from the filter function when chars before the
candidate are added or deleted."
  (let (
        (begin (swiper--get-begin item))
        (end (swiper--get-end item))
        (line-begin (swiper--get-line-begin item))
        (line-end (swiper--get-line-end item))
        )
    (put-text-property
     0 1 'region-data (cons
                       (swiper--async-create-marker (+ end chars-diff))
                       (swiper--async-create-marker
                        (+ begin chars-diff))) item)
    (put-text-property
     0 1 'line-region-data (cons
                            (swiper--async-create-marker (+ line-end chars-diff))
                            (swiper--async-create-marker
                             (+ line-begin chars-diff))) item)))


(defvar ivy--orig-cands nil
  "Store the candidates found with the to-search phrase.")

(defvar ivy--last-cand nil
  "Store the last inserted candidate for faster insertion sort.")
(defvar ivy--next-cand-index 0
  "Store the last inserted candidate index so new candidates will have the correct index.")

(defun swiper--async-iterate-matches (regex beg end func)
  "Iterated over all the matching locations of `regexp' between `beg' and `pos'
and calls `func' for each match."
  (when (< beg end)
    (goto-char beg)
    (while (re-search-forward
            regex
            end
            'on-error-go-to-limit)
      (funcall func (match-beginning 0) (match-end 0)))))

(defun swiper--async-make-startwith-match (re-str)
  "Makes sure the re-str is a startwith pattern (has ^ at its beginning).
This is used when we know the candidate's beginning location and we want to
know where it ends using the searched pattern."
  (if (string-prefix-p "^" re-str) re-str (concat "^" re-str "")))

(defun swiper--async-match (re item)
  "Returns the beginning and the end of a match of `re' inside the string `item'. `re' might be a list of patterns or one pattern not in a list."
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
  "checks if the `item' matches the given `re'. `re' might be a list of patterns
or one pattern not in a list."
  (not (null (save-match-data (swiper--async-match re item)))))

(defun swiper--async-filter (buffer change-begin inserted-end deleted-length)
  "A filter installed on the searched buffer the recognize added or deleted
matches."
  (let (
        (re-str swiper--async-ivy-text-re)
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
           swiper--async-to-search-positive-re search-start search-end
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
              (setcdr first-item tail-items))
            (dolist (to-update last-item)
              (swiper--async-move to-update chars-diff)))))
      (when (>= ivy--index change-index)
        (if (>= deleted-matches (- ivy--index change-index))
            (setq ivy--index change-index)
          (setq ivy--index (- ivy--index deleted-matches)))
        (setq ivy--index (+ ivy--index found-matches)))
        (swiper--async-update-output))
    (setq counsel--async-time (current-time))))

(defun swiper-async-after-change(begin end deleted-length)
  "The hook for after change."
  (save-excursion
    (swiper--async-filter (current-buffer) begin end deleted-length)))

(defun swiper-async-after-change-prop(begin end)
  "The hook for after change string property in the buffer."
  (save-excursion
    (swiper--async-filter (current-buffer) begin end 0)))

(defun swiper--async-is-valid-input ()
  "verifies the swiper input is valid - meaning its not empty
and for regexp function which is regexp - it verifies the re is a valid pattern."
  (and (/= (length ivy-text) 0)
       (or (not (eq 'swiper--regexp-builder ivy--regex-function))
           (swiper--async-legal-pcre-regex-p ivy-text))))

(defcustom swiper--async-grep-limit 2
  "The minimum letter count required for searching using grep.
To disable grep put here a large number, like 999."
  :type 'integer)
(defun swiper--async-same-as-disk()
  "Checks all requirements for using grep. This function is called many times
at runtime to check if we can start using it or to check for detected problems."
  (and (>= (length ivy-text) swiper--async-grep-limit)
       (not (null counsel-grep-command))
       (buffer-file-name)
       (null (funcall buffer-stale-function t))
       (not (buffer-modified-p))))

(defun swiper--async-process-sentinel (process _msg)
  "Sentinel function for an asynchronous grep PROCESS."
  (when (eq (process-status process) 'exit)
    (if (zerop (process-exit-status process))
        (progn
          (swiper--async-parse-process-output process)
          (setq counsel-grep-last-line nil)
          (setq swiper--async-low-start-point swiper--async-low-end-point)
          (setq swiper--async-high-start-point swiper--async-high-end-point)
          (setq swiper--async-process-buffer-processed-point nil)
          (when counsel--async-start
            (setq counsel--async-duration
                  (time-to-seconds (time-since counsel--async-start)))))
      (let (
            (status (process-exit-status process))
            (plist (plist-get counsel--async-exit-code-plist
                              (ivy-state-caller ivy-last)))
            )
        (let (
              (plist-status (plist-get plist status))
              )
          (when (or (null plist-status)
                    (= (plist-get plist status) 0))
            (let ((inhibit-message t))
              (message "swiper-async: disabled grep because of process error"))
            (setq counsel-grep-command nil)))))))

(defvar swiper--async-process-candidates nil
  "Saves candidates offsets when using grep. The process filter stores
values here and the async function reads them.")
(defvar swiper--async-process-last-inserted nil
  "The last inserted candidate from the grep process for faster insertion.")
(defvar swiper--async-process-buffer-processed-point nil
  "Saved the point where the buffer process has stopped.")

(defun swiper--async-parse-process-output (process)
  "The output of grep gives location:matched-result. We parse this as
candidate beginning position and candidate length and calculate the candidate
end from both."
  (let (
        (last-end nil)
        (has-unparsed nil)
        )
    (with-current-buffer (process-buffer process)
      (save-excursion
        (when (null swiper--async-process-buffer-processed-point)
          (setq swiper--async-process-buffer-processed-point (point-min)))
        (goto-char swiper--async-process-buffer-processed-point)
        (while (and (< (point) (point-max))
                    (not (input-pending-p)))
          (setq swiper--async-process-buffer-processed-point (point))
          (let (
                (beg (thing-at-point 'number))
                (bbeg (progn (search-forward ":" nil 'no-exception) (point)))
                (newline-position
                 (progn (search-forward "\n" nil 'no-exception) (point)))
                )
            (if (or (null newline-position)
                    (null bbeg)
                    (null beg))
                (progn
                  (setq has-unparsed t)
                  (goto-char (point-max))) ; should already be there, but why not.
              (let (
                    (len (- newline-position bbeg))
                    )
                (let (
                      (end (+ beg len))
                      )
                  (with-ivy-window
                    (setq beg (filepos-to-bufferpos beg))
                    (setq end (filepos-to-bufferpos end)))
                  (when (or (and (>= beg swiper--async-low-start-point)
                                 (<= beg swiper--async-low-end-point))
                            (and (>= beg swiper--async-high-start-point)
                                 (<= beg swiper--async-high-end-point)))
                    (setq last-end end)
                    (if (not (null swiper--async-process-last-inserted))
                        (let (
                              (new-beg-end (list (cons beg end)))
                              )
                          (setcdr swiper--async-process-last-inserted new-beg-end)
                          (setq swiper--async-process-last-inserted new-beg-end))
                      (push (cons beg end) swiper--async-process-candidates)
                      (setq swiper--async-process-last-inserted
                            swiper--async-process-candidates))))))))))
    (unless (null last-end)
      (if (> last-end swiper--async-high-start-point)
          (setq swiper--async-high-start-point last-end)
        (when (> last-end swiper--async-low-start-point)
          (setq swiper--async-low-start-point last-end)))
      (swiper--async-kick-async))))

(defun swiper--async-process-filter (process str)
  "Receive from PROCESS the output STR.
Update the minibuffer with the amount of lines collected every
`counsel-async-filter-update-time' microseconds since the last update."
  (with-current-buffer (process-buffer process)
    (insert str))
  (when (time-less-p (list 0 0 counsel-async-filter-update-time)
                     (time-since counsel--async-time))
    (swiper--async-parse-process-output process)
    (setq counsel--async-time (current-time))))

(defun swiper--async-call-counsel-grep()
  "Starts the grep process."
  (let (
        (regex (counsel--elisp-to-pcre swiper--async-to-search-re))
        )
    (counsel--async-command
     (format counsel-grep-command (shell-quote-argument regex))
     'swiper--async-process-sentinel
     'swiper--async-process-filter
     swiper--async-process-name)))


(defvar swiper--async-to-search nil
  "Saves the pattern to be search by the search-forward or grep mechanism.
This is a prefix of length `isearch-swiper-limit' of the real searched pattern")
(defcustom isearch-swiper-limit 3
  "The length of the search pattern to start and use the regular ivy-filter
from"
  :type 'integer)
(defvar ivy-text--persp-variables nil
  "A temporary var used in persp-mode to restore the ivy search with.")
(defvar ivy-index--persp-variables nil
  "A temporary var used in persp-mode to restore the ivy search with.")
(defun swiper-async-function (string)
  "Start searching in the current buffer for STRING."
  (with-ivy-window
    (setq ivy--old-re nil)
    (setq isearch-string ivy-text)
    (unless (= (length ivy-text--persp-variables) 0)
      (setq string ivy-text--persp-variables)
      (setq ivy-text ivy-text--persp-variables)
      (setq ivy--old-text ivy-text--persp-variables)
      (setq ivy-text--persp-variables ""))
    (unless (null ivy-index--persp-variables)
      (setq ivy--index ivy-index--persp-variables)
      (setq ivy-index--persp-variables nil))
    (cond
     ((not (swiper--async-is-valid-input)) (swiper--async-reset-state))
     ((string= ivy-text ivy--old-text) (swiper--async-update-all-candidates nil))
     ((and (not (eq 'swiper--regexp-builder ivy--regex-function))
           (or (<= (length ivy-text) isearch-swiper-limit)
               (= (length swiper--async-to-search) 0)
               (not (string-prefix-p swiper--async-to-search ivy-text))))
      (swiper--async-reset-state)
      (setq swiper--async-to-search
            (if (< (length ivy-text) isearch-swiper-limit)
                ivy-text
              (substring ivy-text 0 isearch-swiper-limit)))
      (swiper--async-build-cache)
      (swiper--async-init)
      (swiper--async-update-all-candidates t))
     ((and (eq 'swiper--regexp-builder ivy--regex-function)
           (or (null swiper--async-to-search)
               (not (string= swiper--async-to-search ivy-text))))
      (swiper--async-reset-state)
      (setq swiper--async-to-search ivy-text)
      (swiper--async-build-cache)
      (swiper--async-init)
      (swiper--async-update-all-candidates t))
     (t
      (setq ivy--last-cand nil)
      (setq ivy--next-cand-index 0)
      (swiper--async-build-cache-ivy-text)
      (swiper--async-kick-async)
      (swiper--async-update-all-candidates t)))))

(defvar swiper--async-to-search-old nil
  "A helper used to save the last to-search input pattern.")
(defvar swiper--async-to-search-re nil
  "The last to-search search pattern")
(defun swiper--async-build-cache-to-search()
  "Caches the to-search pattern and create an re of it."
  (unless (string= swiper--async-to-search swiper--async-to-search-old)
    (setq swiper--async-to-search-old swiper--async-to-search)
    (setq swiper--async-to-search-re
          (funcall ivy--regex-function swiper--async-to-search))
    (setq swiper--async-to-search-positive-re
          (swiper-async--join-re-positive swiper--async-to-search-re))))

(defvar swiper--async-ivy-text-old nil
  "A helper used to save the last search input pattern.")
(defvar swiper--async-ivy-text-re nil
  "The last search search pattern")
(defun swiper--async-build-cache-ivy-text()
  "Caches the search pattern and create an re of it."
  (unless (string= swiper--async-ivy-text-old ivy-text)
    (setq swiper--async-ivy-text-old ivy-text)
    (setq swiper--async-ivy-text-re (funcall ivy--regex-function ivy-text))
    (setq swiper--async-ivy-text-positive-re
          (swiper-async--join-re-positive swiper--async-ivy-text-re))))

(defun swiper--async-build-cache()
  "Builds the cache for search and to-search patterns."
  (swiper--async-build-cache-to-search)
  (swiper--async-build-cache-ivy-text))

(defun swiper--async-update-all-candidates(&optional follow-ivy-index)
  "filters all given candidates from `ivy--orig-cands' with the expected re.
FOLLOW-IVY-INDEX changes ivy--index to be the first candidate after swiper--opoint."
  (setq ivy--all-candidates
        (let (
              (re-list swiper--async-ivy-text-re)
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

(defun swiper--async-add-hooks()
  "Adds hooks needed by swiper-async."
  (add-hook 'after-change-functions #'swiper-async-after-change t t)
  (add-hook 'modification-hooks #'swiper-async-after-change-prop t t)
  (add-hook 'window-scroll-functions #'swiper--async-update-input-ivy-scroll-hook t t)
  ;; (add-hook 'window-configuration-change-hook #'swiper--async-update-input-ivy-hook t t)
  (add-hook 'window-size-change-functions #'swiper--async-update-input-ivy-size-hook t t))

(defun swiper-async--remove-hooks()
  "Removed the hooks swiper-async added previously."
  (remove-hook 'after-change-functions #'swiper-async-after-change t)
  (remove-hook 'modification-hooks #'swiper-async-after-change-prop t)
  (remove-hook 'window-scroll-functions #'swiper--async-update-input-ivy-scroll-hook t)
  (remove-hook 'window-size-change-functions #'swiper--async-update-input-ivy-size-hook t))

(defun swiper-async--cleanup ()
  "Resets all data needed by swiper-async and remove all hook."
  (with-ivy-window
    (swiper--async-reset-state)
    ;; (remove-hook 'window-configuration-change-hook #'swiper--async-update-input-ivy-hook t)
    (swiper-async--remove-hooks)))


(defun swiper--async-swiper--cleanup-hook ()
  "A hook into swiper-cleanup to remove all overlays created by swiper-async."
  (with-ivy-window
    (lazy-highlight-cleanup t)
    (isearch-dehighlight)))
(advice-add 'swiper--cleanup :after #'swiper--async-swiper--cleanup-hook)

(defun swiper-async (&optional initial-input)
  "`isearch' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (swiper--async-ivy initial-input))


(defvar swiper--async-timer nil
  "The timer swiper-async uses to re-call the async search function.")
(defun schedule-isearch(buffer func &optional should-sleep-more)
  "Schedule the search function of swiper-async."
  (when (not (null swiper--async-timer))
    (cancel-timer swiper--async-timer)
    (setq swiper--async-timer nil))
  (setq swiper--async-timer (run-at-time
                             (+ swiper--async-isearch-interval
                                (if should-sleep-more
                                    swiper--async-isearch-sleep-more-time
                                    0))
                             nil 'swiper--async-isearch buffer func)))

(defcustom swiper--async-isearch-interval 0
  "The interval between subsequent async search function calls."
  :type 'number)
(defcustom swiper--async-isearch-sleep-more-time 0.1
  "Increase the sleeping time by this amount when the used has some
input or processes has some output."
  :type 'number)
(defvar swiper--async-high-start-point nil
  "The starting point of the high search range.")
(defvar swiper--async-high-end-point nil
  "The ending point of the high search range.")
(defvar swiper--async-low-start-point nil
  "The starting point of the low search range.")
(defvar swiper--async-low-end-point nil
  "The ending point of the low search range.")
(defvar swiper--async-direction-backward nil
  "Tells swiper if searching forward or backward. Used to tell
the search function which direction to search (search-forward
or search-backward).")
(defcustom swiper--async-default-max-matches-per-search 100
  "Maximum search results within one search function call. Reduce this if the minibuffer
is laggy before the search is finished."
  :type 'integer)
(defvar swiper--max-search-length (* 10 4096)
  "Saves the maximum amount of bytes the search function will search for the candidate in
one iteration.") ; one page?
(defcustom swiper--max-search-time 0.1
  "Soft limit for the execution time of the search function")

(defun swiper--async-update-output ()
  "Tells swiper-async that ivy--orig-cands has changed and update
the minibuffer with the new candidates."
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
  "Keeps the positive regexp from re-str (the ones required for the match to be declared as candidate."
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

(setq swiper--async-old-wnd-cands nil)
(defun ivy--wnd-cands-to-str-hook(orig-fun &rest args)
  "Hook for the ivy function. This checks if line numbers are missing on some of the minibuffer candidates."
  (when (eq (ivy-state-caller ivy-last) 'swiper-async)
    (setq swiper--async-old-wnd-cands (car args))
    (let (
          (zero-line-prefix (format swiper--format-spec 0))
          (has-missing-lines nil)
          )
      (dolist (wndcand swiper--async-old-wnd-cands)
        (when (string-prefix-p zero-line-prefix wndcand)
          (setq has-missing-lines t)))
      (when has-missing-lines
        (swiper--async-kick-async))))
  (apply orig-fun args))
(advice-add 'ivy--wnd-cands-to-str :around #'ivy--wnd-cands-to-str-hook)


(defun swiper--async-should-quit-async ()
  (or (input-pending-p)
      (accept-process-output)))

(defun swiper--async-isearch(buffer func)
  "The search function of swiper-async and its main brain.
This function splits the buffer into two ranges: high and low.
The low range is from (point-min) to swiper-opoint and
the high range is from swiper-opoint to the (point-max) when
swiper-async was started.
Depending of the search direction, the result will be first
searched on the high range (if searching forward) and then
on the low range.
This function also responsible for finding line numbers to
candidates in the minibuffer asynchrounouosly."
  (when (active-minibuffer-window)
    (with-ivy-window
      (save-excursion
        (deactivate-mark)
        (let (
              (should-sleep-more nil)
              (finished-wndcands (not swiper-include-line-number-in-search))
              (filled-minibuffer-candidates
               (or (>= (length ivy--orig-cands) (+ max-mini-window-height
                                                   ivy--index))
                   (and (= swiper--async-high-start-point
                           swiper--async-high-end-point)
                        (= swiper--async-low-start-point
                           swiper--async-low-end-point))))
              (found-grep-candidates
               (not (null swiper--async-process-candidates)))
              )
          (when (and swiper-include-line-number-in-search
                     filled-minibuffer-candidates)
            (setq finished-wndcands t)
            (let (
                  (zero-line-prefix (format swiper--format-spec 0))
                  (should-update-wndcands nil)
                  (new-wnd-cands)
                  )
              (when (null swiper--async-last-line-pos)
                (swiper--async-line-at-pos (point-min) t))
              (dolist (wndcand swiper--async-old-wnd-cands)
                (if (not (string-prefix-p zero-line-prefix wndcand))
                    (push wndcand new-wnd-cands)
                  (let (
                        (line-begin (swiper--get-line-begin wndcand))
                        (overlay-begin (swiper--get-begin wndcand))
                        (overlay-end (swiper--get-end wndcand))
                        )
                    (while (and (not (setq should-sleep-more
                                           (swiper--async-should-quit-async)))
                                (/= line-begin swiper--async-last-line-pos))
                      (swiper--async-line-at-pos
                       (if (> line-begin swiper--async-last-line-pos)
                           (+ swiper--async-last-line-pos
                              (min (- line-begin swiper--async-last-line-pos)
                                   swiper--async-max-line-count-size))
                         (- swiper--async-last-line-pos
                            (min (- swiper--async-last-line-pos line-begin)
                                 swiper--async-max-line-count-size)))))
                    (if (/= line-begin swiper--async-last-line-pos)
                        (progn
                          (push wndcand new-wnd-cands)
                          (setq finished-wndcands nil))
                      (setq should-update-wndcands t)
                      (let (
                            (new-wnd-cand
                             (concat (format swiper--format-spec
                                             swiper--async-last-line)
                                     (substring wndcand
                                                (length zero-line-prefix)
                                                (length wndcand))))
                            )
                        (put-text-property
                         0 1 'region-data
                         (cons
                          (swiper--async-create-marker overlay-end)
                          (swiper--async-create-marker overlay-begin))
                         new-wnd-cand)
                        (put-text-property
                         0 1 'line-region-data
                         (cons
                          nil
                          line-begin)
                         new-wnd-cand)
                        (push new-wnd-cand new-wnd-cands))))))
              (when should-update-wndcands
                (ivy--insert-minibuffer
                 (ivy--wnd-cands-to-str (reverse new-wnd-cands))))))
          (let (
                (matches-found 0)
                (swiper--async-max-matches-per-search
                 (if (< (length ivy--orig-cands)
                        (+ max-mini-window-height ivy--index))
                     (- (+ max-mini-window-height ivy--index)
                        (length ivy--orig-cands))
                   swiper--async-default-max-matches-per-search))
                )
            (when found-grep-candidates
              (let (
                    (candidates-create-time (car (benchmark-and-get-result
                (while (and (not (setq should-sleep-more
                                       (swiper--async-should-quit-async)))
                            (< matches-found
                               swiper--async-max-matches-per-search)
                            (not (null swiper--async-process-candidates)))
                  (cl-incf matches-found)
                  (let (
                        (beg-end (pop swiper--async-process-candidates))
                        )
                    (funcall func (car beg-end) (cdr beg-end))))
                ))))
                (when (null swiper--async-process-candidates)
                  (setq swiper--async-process-last-inserted nil))
                (when (/= matches-found 0)
                  (setq swiper--async-default-max-matches-per-search
                        (ceiling (/ (* matches-found
                                       swiper--max-search-time)
                                    candidates-create-time))))))
            (when (and (/= (length swiper--async-to-search) 0)
                       (not (swiper--async-same-as-disk)))
              (counsel-delete-process swiper--async-process-name)
              (let (
                    (re-str swiper--async-to-search-re)
                    )
                (let (
                      (positive-re swiper--async-to-search-positive-re)
                      (searched-bytes 0)
                      )
                  (let (
                        (matches-found-time (car (benchmark-and-get-result

                  (if (not swiper--async-direction-backward)
                      (progn
                        (when (< swiper--async-high-start-point
                                 swiper--async-high-end-point)
                          (goto-char swiper--async-high-start-point)
                          (while (and (not (setq should-sleep-more
                                                 (swiper--async-should-quit-async)))
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
                          (setq searched-bytes
                                (+ searched-bytes
                                   (- (point) swiper--async-high-start-point)))
                          (setq swiper--async-high-start-point (point)))
                        (when (< swiper--async-low-start-point
                                 swiper--async-low-end-point)
                          (goto-char swiper--async-low-start-point)
                          (while (and (not (setq should-sleep-more
                                                 (swiper--async-should-quit-async)))
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
                          (setq searched-bytes
                                (+ searched-bytes
                                   (- (point) swiper--async-low-start-point)))
                          (setq swiper--async-low-start-point (point))))
                    (progn
                      (when (< swiper--async-low-start-point
                               swiper--async-low-end-point)
                        (goto-char swiper--async-low-end-point)
                        (while (and (not (setq should-sleep-more
                                               (swiper--async-should-quit-async)))
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
                        (setq searched-bytes
                                (+ searched-bytes
                                   (- swiper--async-low-end-point (point))))
                        (setq swiper--async-low-end-point (point)))
                      (when (< swiper--async-high-start-point
                               swiper--async-high-end-point)
                        (goto-char swiper--async-high-end-point)
                        (while (and (not (setq should-sleep-more
                                               (swiper--async-should-quit-async)))
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
                        (setq searched-bytes
                                (+ searched-bytes
                                   (- swiper--async-high-end-point (point))))
                        (setq swiper--async-high-end-point (point)))))
                )))
                        )
                    (when (/= searched-bytes 0)
                      (setq swiper--max-search-length ;(* 10 4096)
                            (ceiling (/ (* searched-bytes
                                           swiper--max-search-time)
                                        matches-found-time))))))))
            (when (/= matches-found 0)
              (swiper--async-update-output)))
          (when (or (not finished-wndcands)
                    (not (null swiper--async-process-candidates))
                    (and (not (swiper--async-same-as-disk))
                         (or (< swiper--async-high-start-point
                                swiper--async-high-end-point)
                             (< swiper--async-low-start-point
                                swiper--async-low-end-point))))
            (schedule-isearch buffer func should-sleep-more)))))))

(defun swiper--async-insertion-sort (candidate-cons comp-func insertion-point)
  "Insert the candidate in `candidate-cons' to the list starting at
`insertion-point' in the right place according to `comp-func'. This function
returned all matched candidates in `insertion-point' before the inserted candidate.
`insertion-point' can point to a middle of a list, and the callee should take care
when `candidate-cons' should be before the first item in `insertion-point'. in that
case, the function will return nil."
  (let (
        (idx nil)
        (candidate (car candidate-cons))
        (re-str swiper--async-ivy-text-re)
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
  "Function to compare a candidate according to its starting point."
  (< (swiper--get-begin c1)
     (swiper--get-begin c2)))

(defun swiper--async-create-candidate (b e)
  "Create a candidate."
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
  "This function takes control when a new candidate was found between
begin `b' and end `e'. It first tries to insert into the `ivy--last-cand',
and if it is smaller than the first item it will try to insert it
directly into `ivy--orig-cands'."
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
                  (run-at-time 0 nil 'swiper--async-update-input-ivy))
              (setcdr candidate-cons ivy--orig-cands)
              (setq ivy--orig-cands candidate-cons))))
        (setq ivy--last-cand candidate-cons)
        (let (
              (re-str swiper--async-ivy-text-re)
              )
          (when (swiper--async-matchp re-str candidate)
            (setq idx (+ ivy--next-cand-index idx))
            (setq ivy--next-cand-index (+ idx 1))
            (when (and (>= ivy--index idx)
                       (> (length ivy--orig-cands) 1))
              (cl-incf ivy--index))))))))

(defun swiper--async-format-spec ()
  "Creates `/swiper--format-spec',"
  (let* ((n-lines (count-lines (point-min) (point-max))))
    (let (
          (width (1+ (floor (log n-lines 10))))
          )
      (when (or (null swiper--width) (/= swiper--width width))
        (setq swiper--width width)
        (setq swiper--format-spec (format "%%-%dd: " swiper--width))))))


(defun swiper--async-init ()
  "Initialize a new async search with a give swiper--opoint."
  (setq counsel--async-time (current-time))
  (setq counsel--async-start counsel--async-time)
  (with-ivy-window
    (setq swiper--async-high-start-point swiper--opoint)
    (setq swiper--async-high-end-point (point-max))
    (setq swiper--async-low-start-point (point-min))
    (setq swiper--async-low-end-point swiper--async-high-start-point)
    (swiper--async-format-spec)
    (if (swiper--async-same-as-disk)
        (swiper--async-call-counsel-grep)
      (swiper--async-kick-async))))

(defun swiper--async-kick-async ()
  "Initialize a new async search."
  (schedule-isearch
   (current-buffer)
   'swiper--async-found-new-candidate))

(defun swiper--async-mark-candidates-in-window ()
  "Tells swiper--async-mark-candidates-in-range where to mark candidates."
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
  "Alist of preferred markers with the marker function.
Markers highlights the results in the buffer itself."
  :type '(alist :key-type function :value-type string))

(defun swiper--async-mark-candidates-in-range (beg end)
  "Call the corresponding marker-function,"
  (setq ivy--marker-function
        (or (cdr (assq ivy--regex-function ivy-marker-functions-alist))
            #'swiper--async-mark-candidates-in-range-ivy))
  (when (swiper--async-is-valid-input)
    (funcall ivy--marker-function beg end)))

(defun swiper--async-mark-candidates-in-range-ivy (beg end)
  (when (> (length (ivy-state-current ivy-last)) 0)
    (let (
          (re-str swiper--async-ivy-text-re)
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
     swiper--async-ivy-text-positive-re beg end
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

(defvar isearch-lazy-highlight-buffer nil
  "In not sure what it does,")
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
              ;; XXX : a bug here with invalid marker when executed from
              ;; XXX : ivy--exhibit from timer and using persp-mode.
              ;; XXX : exit gracefully, but need a better way of stoping
              ;; XXX : the exhibit.
              (res (condition-case nil
                       (swiper--async-match-in-buffer item) (error (progn nil))))
              )
          (unless (null res)
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
                (isearch-highlight beg pos)))))))
    (swiper--async-mark-candidates-in-window)))

(defvar swiper--async-process-name "*swiper--async*"
  "Saves the buffer name of the grep process.")
(defun swiper--async-reset-state ()
  "Reset the state swiper-async is using."
  (setq swiper--async-process-buffer-processed-point nil)
  (setq swiper--async-process-last-inserted nil)
  (setq swiper--async-process-candidates nil)
  (setq swiper--async-old-wnd-cands nil)
  (setq counsel-grep-last-line nil)
  (setq ivy-text--persp-variables nil)
  (setq ivy-index--persp-variables nil)
  (setq swiper--async-to-search nil)
  (setq ivy--old-cands nil)
  (setq ivy--all-candidates nil)
  (setq ivy--orig-cands nil)
  (setq ivy--last-cand nil)
  (setq ivy--next-cand-index 0)
  (setq swiper--async-last-line nil)
  (setq swiper--async-last-line-pos nil)
  (setq swiper--async-to-search-old nil)
  (setq swiper--async-to-search-re nil)
  (setq swiper--async-ivy-text-old nil)
  (setq swiper--async-ivy-text-re nil)
  (setq ivy--index 0)
  (setq swiper-use-visual-line nil)
  (counsel-delete-process swiper--async-process-name)
  (when (not (null swiper--async-timer))
    (cancel-timer swiper--async-timer)
    (setq swiper--async-timer nil))
  (swiper--cleanup))

(defvar swiper--async-grep-base-command (concat "grep -a -o -b -u -i -E -e %s %s")
  "Saves the command line arguments grep needs.")

(defun swiper--async-ivy (&optional initial-input)
  "Select one of CANDIDATES and move there.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (swiper--init)
  (setq swiper-invocation-face
        (plist-get (text-properties-at (point)) 'face))
  (let ((preselect nil)
        (minibuffer-allow-text-properties t)
        res)
    (swiper--async-reset-state)
    (swiper--async-add-hooks)
    (if (and buffer-file-name
             (not (ignore-errors (file-remote-p buffer-file-name)))
             (not (jka-compr-get-compression-info buffer-file-name))
             (not (<= (buffer-size)
                      (/ counsel-grep-swiper-limit
                         (if (eq major-mode 'org-mode) 4 1))))
             (condition-case err
                 (counsel-require-program
                  (car (split-string swiper--async-grep-base-command)))
               (user-error (message "no grep: %S" err) nil)))
        (setq counsel-grep-command
              (format swiper--async-grep-base-command
                      "%s" (shell-quote-argument
                            (file-name-nondirectory
                             buffer-file-name))))
      (setq counsel-grep-command nil))
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
                 :history 'counsel-git-grep-history
                 :sort nil
                 :caller 'swiper-async))
          (point))
      (unless (or res swiper-stay-on-quit)
        (goto-char swiper--opoint))
      (unless (or res (string= ivy-text ""))
        (cl-pushnew ivy-text counsel-git-grep-history))
      (when swiper--reveal-mode
        (reveal-mode 1)))))

(defun ivy-rotate-preferred-builders-update()
  "Select a new re-builder and update the prompt so the user will understand on which command he is."
  (setq ivy--old-text nil) ; force re-running dynamic function.
  (swiper-async-function ivy-text)
  (setq ivy--highlight-function
        (or (cdr (assq ivy--regex-function ivy-highlight-functions-alist))
              #'ivy--highlight-default))
  (setq ivy--prompt (ivy-add-prompt-count
                     (ivy--quote-format-string
                      (or (ivy-state-prompt ivy-last) "")))))
(advice-add 'ivy-rotate-preferred-builders :after #'ivy-rotate-preferred-builders-update)

(defun swiper--async-which-func-update ()
  "Update the function name on which-function when swiper-async
is switching between candidates."
  (with-ivy-window
    (which-func-update-1 (selected-window))))
(advice-add 'swiper--async-update-input-ivy :after #'swiper--async-which-func-update)

(require 'pcre2el)

(defun swiper--async-legal-pcre-regex-p (x)
  "Return t if STR is valid regular expression."
  (condition-case nil
      (progn
        (rxt-pcre-to-elisp x)
        t)
    (rxt-invalid-regexp nil))) ; invalid-regexp, error

(defun swiper--regexp-builder (x) (rxt-pcre-to-elisp x))

(add-to-list 'ivy-preferred-re-builders '(regexp-quote . "text"))
(add-to-list 'ivy-preferred-re-builders '(swiper--regexp-builder . "regexp"))



(defun ivy--regex-fuzzy (str)
  "Build a regex sequence from STR.
Insert .* between each char.

This is a fix from the official repo which does not exist on the current emacs."
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

(defun swiper-async-search-forward ()
  (interactive)
  (setq swiper--async-direction-backward nil)
  (swiper-async))

(defun swiper-async-search-backward ()
  (interactive)
  (setq swiper--async-direction-backward t)
  (swiper-async))

(ivy-set-occur 'swiper-async 'counsel-grep-occur)

(provide 'swiper-async)
