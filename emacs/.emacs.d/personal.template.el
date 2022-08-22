(defun nik/find-XXXXXX ()
  (interactive)
  (consult-fd "path"))

(defun nik/grep-XXXXXX ()
  (interactive)
  (consult-ripgrep "path"))

(general-define-key
 :keymaps 'nik/spc
 "," #'nik/grep-XXXXXX
 "." #'nik/find-XXXXXX
 "<" #'nik/grep-XXXXXX
 ">" #'nik/find-XXXXXX
 )

(load-file "XXX/src/tools/emacs/gn.el")

(defun nik/--gn-refs--get-target (f)
  (let ((dir (string-trim-right (file-name-directory f) "/"))
        (file (file-name-nondirectory f))
        )
    (cond
     ((equal file "BUILD.gn")
      (read-string "gn refs: " (concat dir ":" (current-word))))
     (t f))))


(defun nik/gn-refs ()
  (interactive)
  (async-shell-command
   (concat
    "cd XXXX && gn refs XXXX '"
    (nik/--gn-refs--get-target (buffer-file-name))
    "'")) )

(general-define-key
 :keymaps 'nik/spc
 :prefix "i"
 "r" #'nik/gn-refs
 )
