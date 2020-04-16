;; (require 'gdb)
(defun gud-gdb-marker-filter-hook(orig-fun &rest args)
  (let (
        (res (apply orig-fun args))
        )
    (ansi-color-process-output res)
    )
)


; TODO use gdb-goto-breakpoint from gdb-mi
(defun* mystoppedfun(res)
    ;(message "STOPPED %S" res)
;    (message (format "gdb-thread-num %S gdb-frame-number %S gud running %S, gdb update gud running %S gdb show run p %S gdb show stop %S #running %S #stopped %S" gdb-thread-number gdb-frame-number gud-running (gdb-update-gud-running) (gdb-show-run-p) (gdb-show-stop-p) gdb-running-threads-count gdb-stopped-threads-count))

    (gud-refresh)
    (condition-case nil
    (let (
        (my-frame (bindat-get-field res 'frame))
        )
;            (message "frame %S" my-frame)
            (condition-case nil
                (let(
                    (file (bindat-get-field my-frame 'fullname))
                    (line-number (bindat-get-field my-frame 'line))
                    )
;                        (message "file %S" file)
;                        (message "line is %S" line-number)

                        (let (
                            (my-buffer (find-file-noselect file))
                            )
;                                (message "buffer is %S" my-buffer)
                                (let (
                                    (my-window (display-buffer my-buffer))
                                    )
;                                        (message "window is %S" my-window)
                                        (with-selected-window my-window
                                            (goto-line (string-to-number line-number))
;                                            (message "went to line")
;                                            (set-window-point my-window cur-point)
                                            (winstack-push nil t)
                                        )

                                )
                        )
                )
            )
    )
    (error nil))
;    (message "done stop handler")

    )

(with-eval-after-load 'gdb
  (advice-add 'gud-gdb-marker-filter :around #'gud-gdb-marker-filter-hook)
  ;; (setq gdb-command-name "lispgdb")
  (setq gdb-stopped-functions '(mystoppedfun)))

;(let (
;      (gpob (gdb-get-buffer-create 'gdb-partial-output-buffer))
;      )
;  (with-current-buffer gpob
;    (buffer-disable-undo gpob)
;    )
;  )

(provide 'gdb-ext)
