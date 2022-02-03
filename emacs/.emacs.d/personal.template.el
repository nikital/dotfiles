(defun nik/find-XXXXXX ()
  (interactive)
  (consult-fd ""))

(defun nik/grep-XXXXXX ()
  (interactive)
  (consult-ripgrep ""))

(general-define-key
 :keymaps 'nik/spc
 "," #'nik/grep-XXXXXX
 "." #'nik/find-XXXXXX
 "<" #'nik/grep-XXXXXX
 ">" #'nik/find-XXXXXX
 )
