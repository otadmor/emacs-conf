(require 'python-mode)

;; switch to the interpreter after executing code
(setq py-split-window-on-execute nil)
(setq py-switch-buffers-on-execute-p nil)
(setq py-shell-prompt-read-only nil)
;; don't split windows
(py-split-window-on-execute-off)
;; try to automagically figure out indentation
;;(setq py-smart-indentation t)


(defun py--send-string-return-output (strg &optional process msg)
  "Send STRING to PROCESS and return output.

When MSG is non-nil messages the first line of STRING.  Return
the output."
  (let ((process (or process (get-buffer-process (py-shell)))))
    (with-current-buffer (process-buffer process)
      (let* ((erg "")
	     (comint-preoutput-filter-functions
	      (append comint-preoutput-filter-functions
		      '(ansi-color-filter-apply
			(lambda (strg)
			  (progn (setq erg (concat erg strg)) ""))))))
	(py-send-string strg process)
	(accept-process-output process 5)
	(sit-for 0.1 t)
	(when (and erg (not (string= "" erg)))
	  (setq erg
		(replace-regexp-in-string
		 (format "[ \n]*%s[ \n]*" py-fast-filter-re)
		 "" erg)))
	;; (sit-for 0.1 t)
	erg))))

(defun new-python-in-dir(dir) (interactive)
  (let (
        (b (generate-new-buffer "*Python*"))
        )
    (switch-to-buffer b)
    (setq default-directory dir)
    (with-temp-buffer
      (python :fast nil :buffer (buffer-name b) :shell (py-choose-shell nil nil)))
    (let (
          (proc (get-buffer-process (get-buffer b)))
          )
      (py-send-string py-shell-completion-setup-code proc))))


(defun new-python-in-buffer(buffer) (interactive)
  (with-temp-buffer ; py-shell requires the expected buffer not to be the current buffer
    (python :fast nil
            :buffer (buffer-name buffer)
            :shell (py-choose-shell nil nil))
    (let (
          (proc (get-buffer-process buffer))
          )
      (py-send-string py-shell-completion-setup-code proc)))
  buffer)

(defun new-python() (interactive)
       (let (
              (buffer (python :fast nil))
              )
         (let (
               (proc (get-buffer-process (get-buffer buffer)))
               )
           (py-send-string py-shell-completion-setup-code proc))
         (switch-to-buffer buffer)))
(defun old-python() (interactive)
       (let (
             (pyb (car (get-buffers-with-major-mode 'py-python-shell-mode)))
             )
         (if (eq pyb nil)
             (new-python)
           (switch-to-buffer pyb))))

(provide 'python-ext)
