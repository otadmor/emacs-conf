;; -*- lexical-binding: t; -*-

(with-eval-after-load 'ediff-util
  (defun ediff-operate-on-windows-func (func &rest args)
    ;; make sure windows aren't dead
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
           (three-way ediff-3way-job)
           (with-Ancestor (and ediff-merge-with-ancestor-job ediff-show-ancestor)))

      (select-window wind-A)
      (condition-case nil
          (apply func args)
        (error))
      (select-window wind-B)
      (condition-case nil
          (apply func args)
        (error))
      (if three-way
          (progn
            (select-window wind-C)
            (condition-case nil
                (apply func args)
              (error))))
      (when with-Ancestor
        (select-window wind-Anc)
        (condition-case nil
            (apply func args)
          (error)))
      (select-window wind)))

  (defun ediff-wrap-interactive (func)
    ;; (interactive "P")
    (lambda (&rest args)
      (interactive (advice-eval-interactive-spec
                    (cadr (interactive-form func))))
      (ediff-barf-if-not-control-buffer)

      ;; make sure windows aren't dead
      (if (not (and (window-live-p ediff-window-A) (window-live-p ediff-window-B)))
          (ediff-recenter 'no-rehighlight))
      (if (not (and (ediff-buffer-live-p ediff-buffer-A)
                    (ediff-buffer-live-p ediff-buffer-B)
                    (or (not ediff-3way-job)
                        (ediff-buffer-live-p ediff-buffer-C))
                    (or (not ediff-merge-with-ancestor-job)
                        (not ediff-show-ancestor)
                        (ediff-buffer-live-p ediff-ancestor-buffer))
                    ))
          (error ediff-KILLED-VITAL-BUFFER))
      (apply 'ediff-operate-on-windows-func
             (cons func args)))))

(with-eval-after-load 'ediff
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-toggle-multiframe nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  )

(provide 'ediff-ext)
