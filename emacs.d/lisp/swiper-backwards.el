;;; `swiper-backwards': https://github.com/abo-abo/swiper/issues/412
;;;
;;; * Select closest match backwards from point instead of closest match
;;;   forwards
;;;
;;; * Reverse `evil-ex-search-direction' (flipping `evil-ex-search-next' and
;;;   `evil-ex-search-previous')

(defun brds/ivy-recompute-index-swiper-backward (_re-str cands)
  ;; still unsure how I feel about the oddness of `1-'
  (1- (ivy-recompute-index-swiper _re-str cands)))

(defun brds/swiper-backward (&optional initial-input)
  (interactive)
  (let ((ivy-index-functions-alist
         '((swiper . brds/ivy-recompute-index-swiper-backward))))
    (swiper initial-input)))

;; also requires the additon of `brds/ivy-recompute-index-swiper-backward'
;; to this function alongside `ivy-recompute-index-swiper' and -asnyc
(defun ivy--filter (name candidates)
  "Return all items that match NAME in CANDIDATES.
CANDIDATES are assumed to be static."
  (let ((re (funcall ivy--regex-function name)))
    (if (and (equal re ivy--old-re)
             ivy--old-cands)
        ;; quick caching for "C-n", "C-p" etc.
        ivy--old-cands
      (let* ((re-str (if (listp re) (caar re) re))
             (matcher (ivy-state-matcher ivy-last))
             (case-fold-search
              (and ivy-case-fold-search
                   (or (eq ivy-case-fold-search 'always)
                       (string= name (downcase name)))))
             (cands (cond
                     (matcher
                      (funcall matcher re candidates))
                     ((and ivy--old-re
                           (stringp re)
                           (stringp ivy--old-re)
                           (not (string-match "\\\\" ivy--old-re))
                           (not (equal ivy--old-re ""))
                           (memq (cl-search
                                  (if (string-match "\\\\)\\'" ivy--old-re)
                                      (substring ivy--old-re 0 -2)
                                    ivy--old-re)
                                  re)
                                 '(0 2)))
                      (ignore-errors
                        (cl-remove-if-not
                         (lambda (x) (string-match re x))
                         ivy--old-cands)))
                     (t
                      (ivy--re-filter re candidates)))))
        (if (memq (cdr (assoc (ivy-state-caller ivy-last)
                              ivy-index-functions-alist))
                  '(brds/ivy-recompute-index-swiper-backward
                    ivy-recompute-index-swiper
                    ivy-recompute-index-swiper-async))
            (progn
              (ivy--recompute-index name re-str cands)
              (setq ivy--old-cands (ivy--sort name cands)))
          (setq ivy--old-cands (ivy--sort name cands))
          (ivy--recompute-index name re-str ivy--old-cands))
        (setq ivy--old-re
              (if (eq ivy--highlight-function 'ivy--highlight-ignore-order)
                  re
                (if ivy--old-cands
                    re-str
                  "")))
        ivy--old-cands))))














(defun tv/ivy-recompute-index-swiper-backward (_re-str cands)
  (- (ivy-recompute-index-swiper _re-str cands) 1))

(push (cons 'tv/swiper-backward 'tv/ivy-recompute-index-swiper-backward)
      ivy-index-functions-alist)

(defun tv/swiper-backward--ivy (candidates &optional initial-input)
  (interactive)
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
    (unwind-protect
        (and
         (setq res
               (ivy-read
                "Swiper: "
                candidates
                :initial-input initial-input
                :keymap swiper-map
                :preselect preselect
                :require-match t
                :update-fn #'swiper--update-input-ivy
                :unwind #'swiper--cleanup
                :action #'swiper--action
                :re-builder #'swiper--re-builder
                :history 'swiper-history
                :caller 'tv/swiper-backward))
         (point))
      (unless res
        (goto-char swiper--opoint)))))

(defun tv/swiper-backward (&optional initial-input)
  (interactive)
  (tv/swiper-backward--ivy (swiper--candidates) initial-input)
  )

(defun tv/swiper-backward (&optional initial-input)
  (interactive)
  (let ((ivy-index-functions-alist
         '((swiper . tv/ivy-recompute-index-swiper-backward))))
    (swiper initial-input)))
