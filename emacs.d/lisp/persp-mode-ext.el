(require 'persp-mode)

;(setq persp-autokill-buffer-on-remove nil)
(setq persp-auto-resume-time 0.01)
(setq persp-auto-save-fname "autosave")
; (setq persp-auto-save-opt 1)
(setq persp-nil-hidden t)
(setq persp-nil-name "nil")
(setq persp-add-buffer-on-after-change-major-mode 'free)

(defun persp-buffer-name-from-savelist (savelist)
  (destructuring-bind (buffer-name vars-list &rest _rest) (cdr savelist)
    buffer-name))

(defun persp-load-variables-from-savelist (savelist)
  (destructuring-bind (buffer-name vars-list &rest _rest) (cdr savelist)
    (mapc #'(lambda (varcons)
              (destructuring-bind (vname . vvalue) varcons
                (unless (or (eq vname 'buffer-file-name)
                            (eq vname 'major-mode))
                  (set (make-local-variable vname) vvalue))))
          vars-list)
    buffer-name))

(defun persp-load-get-default-directory-from-savelist (savelist)
  (destructuring-bind (buffer-name vars-list &rest _rest) (cdr savelist)
    (alist-get 'default-directory vars-list)))

(defun persp-is-same-major-mode-from-savelist (savelist mode)
  (destructuring-bind (buffer-name vars-list &rest _rest) (cdr savelist)
    (let ((buf-mmode (alist-get 'major-mode vars-list)))
      (eq buf-mmode mode))))

(def-persp-buffer-save/load
 :mode 'py-python-shell-mode
 :load-function #'(lambda (savelist default-load-function after-load-function)
                    (when (persp-is-same-major-mode-from-savelist
                           savelist 'py-python-shell-mode)
                      (let (
                            (buffer-name (persp-buffer-name-from-savelist savelist))
                            )
                        (let (
                              (buffer (get-buffer-create buffer-name))
                              )
                          (with-current-buffer buffer
                            (persp-load-variables-from-savelist savelist))
                          (new-python-in-buffer buffer)
                          buffer))))
 :save-vars '(default-directory))


(def-persp-buffer-save/load
 :mode 'shell-mode
 :load-function #'(lambda (savelist default-load-function after-load-function)
                    (when (persp-is-same-major-mode-from-savelist
                           savelist 'shell-mode)
                      (let (
                            (buffer-name (persp-buffer-name-from-savelist savelist))
                            )
                        (let (
                              (buffer (get-buffer-create buffer-name))
                              )
                          (with-current-buffer buffer
                            (persp-load-variables-from-savelist savelist)
                            (shell buffer))
                          buffer))))
 :save-vars '(default-directory))

;; magit-status
(with-eval-after-load "magit-autoloads"
  (autoload 'magit-status-mode "magit")
  (autoload 'magit-refresh "magit")
  (def-persp-buffer-save/load
   :mode 'magit-status-mode
   :load-function #'(lambda (savelist default-load-function after-load-function)
                      (when (persp-is-same-major-mode-from-savelist
                             savelist 'magit-status-mode)
                        (let (
                              (magit-directory
                               (persp-load-get-default-directory-from-savelist
                                savelist))
                              )
                          (let (
                                (magit-buffer)
                                )
                            (let (
                                  (refresh-buffer-hook
                                   #'(lambda ()
                                       (setq magit-buffer (current-buffer))))
                                  )
                              (add-hook 'magit-refresh-buffer-hook
                                        refresh-buffer-hook)
                              (magit-status magit-directory)
                              (remove-hook 'magit-refresh-buffer-hook
                                           refresh-buffer-hook))))))
   :save-vars '(default-directory))

  (add-hook 'persp-common-buffer-filter-functions
            #'(lambda (b) (string-prefix-p "magit-diff" (buffer-name b))))
  (add-hook 'persp-common-buffer-filter-functions
            #'(lambda (b) (string-prefix-p "magit-process" (buffer-name b))))
  )


;; (setq persp-shared-buffers '("*scratch*" "*Messages*" "*Backtrace*"))
;; (add-hook 'persp-activated-functions
;;           #'(lambda (_)
;;               (persp-add-buffer persp-shared-buffers)))

(defun persp-parameters-to-savelist-hide-message (persp)
  `(def-params ,(remove-if
                 #'(lambda (param)
                     (and (not (persp-elisp-object-readable-p param))
                          (progn
                            (unless persp-mode-hide-autosave-errors
                              (message "[persp-mode] Info: The parameter %S \
of the perspective %s can't be saved."
                                       param (safe-persp-name persp)))
                            t)
                          t))
                 (safe-persp-parameters persp))))
(defalias 'persp-parameters-to-savelist 'persp-parameters-to-savelist-hide-message)
(setq persp-mode-hide-autosave-errors t)
(with-eval-after-load "persp-mode"
  (setq wg-morph-on nil)
  (set-persp-parameter 'dont-save-to-file t nil)
  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))

    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil)))))

  (run-with-idle-timer 5 t (lambda ()
                             (let (
                                   (inhibit-message persp-mode-hide-autosave-errors)
                                   (save-silently t)
                                   )
                               (persp-save-state-to-file))))

  ;; switch off the animation of restoring window configuration
  (add-hook 'after-init-hook
            #'(lambda ()
                (progn
                  (persp-mode 1)
                  (run-at-time 0.1 nil 'perspsw1)
                  )))
  )

(add-to-list 'speedbar-frame-parameters (cons 'persp-ignore-wconf t))

(add-hook 'persp-common-buffer-filter-functions
          #'(lambda (b) (string-prefix-p "epc con" (buffer-name b))))
(add-hook 'persp-common-buffer-filter-functions
          #'(lambda (b) (string-prefix-p "epc server" (buffer-name b))))

(provide 'persp-mode-ext)
