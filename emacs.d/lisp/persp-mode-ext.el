;; -*- lexical-binding: t; -*-
;; (require 'persp-mode)

(defun persp-delete-frame-hook(fun &rest args)
  (let (
        (debug-on-error nil)
        )
    (apply fun args)))

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

(setq persp-shared-buffers '("*scratch*" "*Messages*" "*Backtrace*" " *server*" "server.log"))
(defun persp-mode-add-shared-buffers (orig-fun &rest args)
  (let (
        (res (apply orig-fun args))
        (buffer (current-buffer))
        )
    (save-excursion
      (condition-case nil
          (persp-add-buffer persp-shared-buffers)
        (error nil))
      (persp-switch-to-buffer buffer))
    res))

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

(defun switch-to-persp1-after-load-state (persp-file phash persp-names)
  (remove-hook 'persp-after-load-state-functions
               'switch-to-persp1-after-load-state)
  (when server-inside-emacs-client
    (perspsw1)
    (run-with-idle-timer 5 t 'persp-mode-try-save)))

(setq persp-mode-hide-autosave-errors t)

(defun buffer-list-persp-mode ()
  (let (
        (res (cl-remove-if
              (lambda (b)
                (when persp-mode
                  (let ((persp (get-current-persp)))
                    (if persp
                        (not (persp-contain-buffer-p b persp))
                      nil))))
              (buffer-list)))
        )
    (if (null res)
        (list (get-buffer "*Messages*")) ; TODO: use persp-shared-buffers
      res)))

;; copied from https://github.com/emacs-mirror/emacs/blob/master/lisp/mouse.el
(defun mouse-buffer-menu-map-hook ()
  ;; Make an alist of elements that look like (MENU-ITEM . BUFFER).
  (let ((buffers (buffer-list-persp-mode)) split-by-major-mode sum-of-squares)
    (dolist (buf buffers)
      ;; Divide all buffers into buckets for various major modes.
      ;; Each bucket looks like (MODE NAMESTRING BUFFERS...).
      (with-current-buffer buf
        (let* ((adjusted-major-mode major-mode) elt)
          (dolist (group mouse-buffer-menu-mode-groups)
            (when (string-match (car group) (format-mode-line mode-name))
              (setq adjusted-major-mode (cdr group))))
          (setq elt (assoc adjusted-major-mode split-by-major-mode))
          (unless elt
            (setq elt (list adjusted-major-mode
                            (if (stringp adjusted-major-mode)
                                adjusted-major-mode
                              (format-mode-line mode-name nil nil buf)))
                  split-by-major-mode (cons elt split-by-major-mode)))
          (or (memq buf (cdr (cdr elt)))
              (setcdr (cdr elt) (cons buf (cdr (cdr elt))))))))
    ;; Compute the sum of squares of sizes of the major-mode buckets.
    (let ((tail split-by-major-mode))
      (setq sum-of-squares 0)
      (while tail
	(setq sum-of-squares
	      (+ sum-of-squares
		 (let ((len (length (cdr (cdr (car tail)))))) (* len len))))
	(setq tail (cdr tail))))
    (if (< (* sum-of-squares mouse-buffer-menu-mode-mult)
	   (* (length buffers) (length buffers)))
	;; Subdividing by major modes really helps, so let's do it.
	(let (subdivided-menus (buffers-left (length buffers)))
	  ;; Sort the list to put the most popular major modes first.
	  (setq split-by-major-mode
		(sort split-by-major-mode
		      (function (lambda (elt1 elt2)
				  (> (length elt1) (length elt2))))))
	  ;; Make a separate submenu for each major mode
	  ;; that has more than one buffer,
	  ;; unless all the remaining buffers are less than 1/10 of them.
	  (while (and split-by-major-mode
		      (and (> (length (car split-by-major-mode)) 3)
			   (> (* buffers-left 10) (length buffers))))
	    (let ((this-mode-list (mouse-buffer-menu-alist
				   (cdr (cdr (car split-by-major-mode))))))
	      (and this-mode-list
		   (setq subdivided-menus
			 (cons (cons
				(nth 1 (car split-by-major-mode))
				this-mode-list)
			       subdivided-menus))))
	    (setq buffers-left
		  (- buffers-left (length (cdr (car split-by-major-mode)))))
	    (setq split-by-major-mode (cdr split-by-major-mode)))
	  ;; If any major modes are left over,
	  ;; make a single submenu for them.
	  (if split-by-major-mode
	      (let ((others-list
		     (mouse-buffer-menu-alist
		      ;; we don't need split-by-major-mode any more,
		      ;; so we can ditch it with nconc (mapcan).
		      (mapcan 'cddr split-by-major-mode))))
		(and others-list
		     (setq subdivided-menus
			   (cons (cons "Others" others-list)
				 subdivided-menus)))))
          (cons "Buffer Menu" (nreverse subdivided-menus)))
      (cons "Buffer Menu"
            (mouse-buffer-menu-split "Select Buffer"
                                     (mouse-buffer-menu-alist buffers))))))



(defun persp-switch-set-this-command (&rest args)
  (setq this-command 'persp-switch))

(defun persp-mode-try-save ()
  (let (
        (inhibit-message persp-mode-hide-autosave-errors)
        (save-silently t)
        )
    (condition-case err
        (persp-save-state-to-file)
      (error
       (remove-hook 'persp-after-load-state-functions
                    'switch-to-persp1-after-load-state)
       (message "failed to save persp %S" err) nil))))

(with-eval-after-load "persp-mode-autoloads"
  (advice-add 'set-frame-persp :around #'persp-mode-add-shared-buffers)
  (defalias 'persp-parameters-to-savelist 'persp-parameters-to-savelist-hide-message)
  (defalias 'mouse-buffer-menu-map 'mouse-buffer-menu-map-hook)
  (advice-add 'persp-switch :before #'persp-switch-set-this-command)

  ;; switch off the animation of restoring window configuration
  (setq wg-morph-on nil)
  (set-persp-parameter 'dont-save-to-file t nil)

  (with-eval-after-load 'ivy
    (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (if (not (persp-contain-buffer-p b persp))
                              (if (not ivy-use-virtual-buffers)
                                  t
                                (let ((is-virtual-buffer
                                       (eq (get-text-property 0 'face b)
                                           'ivy-virtual)))
                                      (not is-virtual-buffer)))
                            nil)
                        nil)))))

    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil)))))
  (add-hook 'after-init-hook
            (lambda ()
              (add-hook 'persp-after-load-state-functions
                        'switch-to-persp1-after-load-state)
              (when server-inside-emacs-client
                (persp-mode 1))))

  (setq persp-autokill-buffer-on-remove nil)
  (setq persp-auto-resume-time 0.01)
  (setq persp-auto-save-fname "autosave")
  (setq persp-auto-save-opt 2)
  (setq persp-nil-hidden t)
  (setq persp-nil-name "nil")
  (setq persp-add-buffer-on-after-change-major-mode 'free)
  (advice-add 'persp-delete-frame :around #'persp-delete-frame-hook)

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

  (def-persp-buffer-save/load
    :mode 'dired-mode
    :load-function #'(lambda (savelist default-load-function after-load-function)
                       (when (persp-is-same-major-mode-from-savelist
                              savelist 'dired-mode)
                         (let (
                               (buffer
                                (dired
                                 (persp-load-get-default-directory-from-savelist
                                  savelist)))
                               )
                           (let (
                                 (buffer-name
                                  (persp-buffer-name-from-savelist savelist))
                                 )
                             (with-current-buffer buffer
                               (rename-buffer buffer-name)
                               (persp-load-variables-from-savelist savelist))))))
    :save-vars '(default-directory))

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

  (add-hook 'persp-common-buffer-filter-functions
            #'(lambda (b) (string-prefix-p "epc con" (buffer-name b))))
  (add-hook 'persp-common-buffer-filter-functions
            #'(lambda (b) (string-prefix-p "epc server" (buffer-name b))))
  )



(provide 'persp-mode-ext)
