(require 'select)
(cl-defmethod gui-backend-set-selection
  (type data
        &context (window-system nil)
        ((not (frame-parameter (selected-frame) 'display)) (eql nil))
        ;; Only applies to terminals which have it enabled.
        ((terminal-parameter nil 'xterm--get-selection) (eql nil))
        ;; Doesn't work in work in tmux
        ((eq (terminal-parameter nil 'terminal-initted)
             'terminal-init-screen)
         (eql t)))
  (when (not (or (eq type 'CLIPBOARD) (eq type 'PRIMARY) (eq type 'SECONDARY)))
    (error "Unsupported data type %S" type))
  (let* (
         (display (replace-regexp-in-string "\n$" "" (shell-command-to-string "tmux switch-client -r > /dev/null 2>&1 && tmux switch-client -r > /dev/null 2>&1 ; tmux show-env DISPLAY | grep -oP \"(?<==)(.*)\"")))
         (p (make-process :name "xclip" :connection-type 'pipe :noquery t :buffer nil ;"*xclip*"
                          :command (list "xclip"
                                         "-selection"
                                         (cond ((eq type 'CLIPBOARD) "c")
                                               ((eq type 'PRIMARY) "p")
                                               ((eq type 'SECONDARY) "s"))
                                         "-display"
                                         display)))
         )
    (process-send-string p (substring-no-properties data))
    (process-send-eof p)))


(cl-defmethod gui-backend-get-selection
  (type data-type
        &context (window-system nil)
        ((not (frame-parameter (selected-frame) 'display)) (eql nil))
        ;; Only applies to terminals which have it enabled.
        ((terminal-parameter nil 'xterm--get-selection) (eql nil))
        ;; Doesn't work in work in tmux
        ((eq (terminal-parameter nil 'terminal-initted)
             'terminal-init-screen)
         (eql t)))
  (unless (eq data-type 'STRING)
    (error "Unsupported data type %S" data-type))
  (let (
        (display (replace-regexp-in-string "\n$" "" (shell-command-to-string "tmux switch-client -r > /dev/null 2>&1 && tmux switch-client -r > /dev/null 2>&1 ; tmux show-env DISPLAY | grep -oP \"(?<==)(.*)\"")))
        )
    (shell-command-to-string (format "xclip -selection c -o -display %s" display))))

(provide 'xclip)
