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

(defun nik/--extract-filename-from-patch ()
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "diff --git a\\([^[:space:]]*\\)" (line-end-position))
    (match-string-no-properties 1)))

(defun nik/--extract-line-from-patch ()
  (let ((original-line (line-number-at-pos)))
    (save-excursion
      (when (re-search-backward "^@@ -[[:digit:]]*,[[:digit:]]* \\+\\([[:digit:]]*\\)," 1 t)
        (+
         (string-to-number (match-string-no-properties 1)) ;; Beginning of hunk
         (- original-line (1+ (line-number-at-pos))) ;; Offset in hunk
         )))))

(defun nik/goto-patch-dwim ()
  (interactive)
  (let ((root "XXX"))
    (if (equal "patch" (file-name-extension (buffer-file-name)))
        (let ((filename (concat root (nik/--extract-filename-from-patch)))
              (line (nik/--extract-line-from-patch)))
          (find-file-existing filename)
          (when line
            (goto-char (point-min))
            (forward-line (1- line))
            (recenter)))
      (let* ((file (file-relative-name (buffer-file-name) root))
             (patch (concat
                     root
                     "/XXXX/"
                     (replace-regexp-in-string "/" "-" file)
                     ".patch")))
        (find-file-existing patch)))))

(general-define-key
 :keymaps 'nik/spc
 :prefix "i"
 "r" #'nik/gn-refs
 "p" #'nik/goto-patch-dwim
 )
