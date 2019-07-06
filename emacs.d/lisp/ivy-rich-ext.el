;;;  -*- lexical-binding: t -*-

(require 'ivy-rich)
(defun popup-item-summary-or-empty(item)
  (if (null item)
      ""
    (let (
          (data (popup-item-summary item))
          )
      (if (null data)
          ""
        data))))
(defun popup-item-symbol-or-empty(item)
  (if (null item)
      ""
    (let (
          (data (popup-item-symbol item))
          )
      (if (null data)
          ""
        data))))
(add-to-list
 'ivy-rich-display-transformers-list
 '(:columns
   ((ivy-cleanup-string (:width 30))
    (popup-item-summary-or-empty (:width 30 :face font-lock-doc-face))
    (popup-item-symbol-or-empty (:face font-lock-comment-face)))))
(add-to-list
 'ivy-rich-display-transformers-list
 'ivy-completion-in-region)
(ivy-rich-mode 1)

(provide 'ivy-rich-ext)
