(defvar c-files-regex ".*\\.\\(c\\|cpp\\|h\\|hpp\\)"
  "A regular expression to match any c/c++ related files under a directory")
 
(defun my-semantic-parse-dir (root regex)
  "
   This function is an attempt of mine to force semantic to
   parse all source files under a root directory. Arguments:
   -- root: The full path to the root directory
   -- regex: A regular expression against which to match all files in the directory
  "
  (let (
        ;;make sure that root has a trailing slash and is a dir
        (root (file-name-as-directory root))
        (files (directory-files root t ))
       )
    ;; remove current dir and parent dir from list
    (setq files (delete (format "%s." root) files))
    (setq files (delete (format "%s.." root) files))
    ;; remove any known version control directories 
    (setq files (delete (format "%s.git" root) files))
    (setq files (delete (format "%s.hg" root) files))
    (while files
      (setq file (pop files))
      (if (not(file-accessible-directory-p file))
          ;;if it's a file that matches the regex we seek
          (progn (when (string-match-p regex file)
                   (save-excursion
                     (semanticdb-file-table-object file))
           ))
          ;;else if it's a directory
          (my-semantic-parse-dir file regex)
      )
     )
  )
)
 
(defun my-semantic-parse-current-dir (regex)
  "
   Parses all files under the current directory matching regex
  "
  (let (
        (curfile (buffer-file-name))
        )
    (if curfile
        (my-semantic-parse-dir (file-name-directory curfile) regex)
      (my-semantic-parse-dir default-directory regex)
      )
    )
)
 
(defun lk-parse-curdir-c ()
  "
   Parses all the c/c++ related files under the current directory
   and inputs their data into semantic
  "
  (interactive)
  (my-semantic-parse-current-dir c-files-regex)
)
 
(defun lk-parse-dir-c (dir)
  "Prompts the user for a directory and parses all c/c++ related files
   under the directory
  "
  (interactive (list (read-directory-name "Provide the directory to search in:")))
  (my-semantic-parse-dir (expand-file-name dri) c-files-regex)
)
 
(provide 'lk-file-search)
