;; -*- lexical-binding: t; -*-
(require 'swiper)
(require 'ivy)
(require 'multiple-cursors-swiper) ; for candidates with advancer and initiater

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
                        :use-format-mode-line t
                        :include-empty-last-line t)))

(defcustom swiper-async-filter-update-time 50
  "The amount of microseconds to wait until updating `swiper--async-filter'."
  :type 'integer)

(defun swiper--get-line (item)
  (get-text-property 0 'swiper-no-line-number item))
(defun swiper--get-str-line (item)
  (get-text-property 0 'swiper-line-number item))
(defun swiper--get-begin (item)
  (get-text-property 0 'begin item))
(defun swiper--get-end (item)
  (get-text-property 0 'end item))


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

(defun swiper--async-filter (buffer begin end deleted-length)
  "Receive from buffer the output STR.
Update the minibuffer with the amount of lines collected every
`swiper-async-filter-update-time' microseconds since the last update."
  (when (and t
             ;; (time-less-p (list 0 0 swiper-async-filter-update-time)
             ;; (time-since counsel--async-time))
             )
    (setq ivy--old-cands ivy--all-candidates)
    (let (
          (chars-diff (- (- end begin) deleted-length))
          (new-candidates (with-current-buffer buffer (swiper--async-candidates begin end)))
          )
      (let (
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
        (cl-loop for cand in ivy--all-candidates do
                 (when (> (length new-candidates) 0)
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
                                                       drained-length))
                             )
                           )
                         )
                       )
                     )
                   (cl-incf i))
        (when (null first-index)
          (setq first-index (length ivy--all-candidates)))
        (when (null last-index)
          (setq last-index (length ivy--all-candidates)))
        (setq added-lines (+ (- last-index first-index) 1))
        (setq deleted-lines (+ deleted-lines 1))
        (unless (= deleted-leftover 0)
          (message "WARNING - deleted-leftover (=%S) != 0" deleted-leftover))
        (unless (= (length new-candidates) added-lines)
          (message "WARNING - amount of added cands %S != diff of first %S and last %S lines = %S" (length new-candidates) last-index first-index added-lines))
        (let (
              (cand-lasts (nthcdr (+ first-index deleted-lines)
                                  ivy--all-candidates))
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
              (if (= (length new-candidates) 0)
                  (setq new-candidates new-cand-lasts)
                (setcdr (last new-candidates) new-cand-lasts))))))
        (if (> first-index 0)
            (setcdr (nthcdr (- first-index 1) ivy--all-candidates)
                    new-candidates)
          (setq ivy--all-candidates new-candidates))
        (setq ivy--index (+ ivy--index lines-diff))))
    (ivy--set-candidates ivy--all-candidates)
    (setq ivy--old-cands ivy--all-candidates)
    (let (
          (fmt (ivy--format ivy--all-candidates))
          )
      (ivy--insert-minibuffer fmt))
    (setq counsel--async-time (current-time))))

(defun swiper-async-after-change(begin end deleted-length)
  (swiper--async-filter (current-buffer) begin end deleted-length))

(defun swiper-async-function (string)
  "Grep in the current directory for STRING."
  (when (= (length ivy--all-candidates) 0)
    (let ((regex (counsel--elisp-to-pcre
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (setq counsel--async-time (current-time))
      (setq counsel--async-start counsel--async-time)
      (setq swiper-use-visual-line nil)
      (with-ivy-window
        (let (
              (start (point-min))
              (end (point-max))
              )
          (add-hook 'after-change-functions #'swiper-async-after-change t t)
          (swiper-async-after-change start end 0)))))
    (ivy--filter ivy-text ivy--all-candidates))

(defun swiper-async--cleanup ()
  (with-ivy-window
    (remove-hook 'after-change-functions #'swiper-async-after-change t))
  (swiper--cleanup))

(defun swiper-async (&optional initial-input)
  "`isearch' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (swiper--async-ivy initial-input))

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
                 :update-fn #'swiper--update-input-ivy
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
