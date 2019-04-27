(require 'winstack)

(defun winstack-list-action (item)
  (with-ivy-window
    (when (> (length ivy--all-candidates) 0)
      (let* (
             (orig-idx (get-text-property 0 'winstack-idx item))
             (winstack-future-winstack (winstack-winstack-future-get))
             (winstack-winstack (winstack-winstack-get))
             (future-winstack-length (length winstack-future-winstack))
             (winstack-length (length winstack-winstack))
             (effective-idx
              (if (>= orig-idx future-winstack-length)
                  (- orig-idx future-winstack-length)
                (- future-winstack-length orig-idx 1)))
             (winstack-item
              (if (>= orig-idx future-winstack-length)
                  (nth effective-idx winstack-winstack)
                (nth effective-idx winstack-future-winstack)))
             )
        (if (>= orig-idx future-winstack-length)
            (if (= effective-idx 0)
                (winstack-future-stack-push (winstack-stack-pop))
              (let*(
                    (prev-winstack-stack (nthcdr (- effective-idx 1) winstack-winstack))
                    (new-winstack-stack (cdr prev-winstack-stack))
                    )
                (setcdr prev-winstack-stack nil)
                (winstack-winstack-future-set
                 (append (reverse winstack-winstack) winstack-future-winstack '()))
                (winstack-winstack-set new-winstack-stack)))
          (if (= effective-idx 0)
              (winstack-stack-push (winstack-future-stack-pop))
            (let*(
                  (prev-winstack-future-stack
                   (nthcdr (- effective-idx 1) winstack-future-winstack))
                  (new-winstack-future-stack (cdr prev-winstack-future-stack))
                  )
              (setcdr prev-winstack-future-stack nil)
              (winstack-winstack-set
               (append (reverse winstack-future-winstack) winstack-winstack '()))
              (winstack-winstack-future-set new-winstack-future-stack))))
        (jump-to-item winstack-item)))))

(defun winstack-list-update ()
  (with-ivy-window
    (when (> (length ivy--all-candidates) 0)
      (let* (
             (item (ivy-state-current ivy-last))
             (orig-idx (get-text-property 0 'winstack-idx item))
             (winstack-future-winstack (winstack-winstack-future-get))
             (winstack-winstack (winstack-winstack-get))
             (future-winstack-length (length winstack-future-winstack))
             (winstack-length (length winstack-winstack))
             (winstack-item
              (if (>= orig-idx future-winstack-length)
                  (nth (- orig-idx future-winstack-length) winstack-winstack)
                (nth (- future-winstack-length orig-idx 1) winstack-future-winstack)))
             )
        (jump-to-item winstack-item)))))

(defun ivy-winstack-list ()
  (interactive)
  (let* (
        (future (reverse (winstack-winstack-future-get)))
        (past (winstack-winstack-get))
        (preselect (length future))
        (joined-winstack (append future past '()))
        (candidates
         (let (
               (idx 0)
               )
           (cl-mapcar
            (lambda (item)
              (let (
                    (str (format "%s:%d" (third item)
                                 (winstack-point-from-marker (fourth item))))
                    )
                (put-text-property
                 0 1 'winstack-idx
                 idx str)
                (cl-incf idx)
                str))
            joined-winstack)))
        )
    (ivy-read "Winstack:"
              candidates
              :preselect (length future)
              :update-fn #'winstack-list-update
              :action #'winstack-list-action
              )
    ))
(advice-add 'ivy-winstack-list :around #'disable-winstack-hook)


(provide 'winstack-list)
