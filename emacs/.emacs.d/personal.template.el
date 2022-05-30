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
