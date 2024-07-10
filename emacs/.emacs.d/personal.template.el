;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(defun nik/find-XXXXXX ()
  (interactive)
  (consult-fd "XXXXXX"))

(defun nik/grep-XXXXXX ()
  (interactive)
  (consult-ripgrep "XXXXXX"))

(general-define-key
 :keymaps 'nik/spc
 "," #'nik/grep-XXXXXX
 "." #'nik/find-XXXXXX
 "<" #'nik/grep-XXXXXX
 ">" #'nik/find-XXXXXX
 )

(load-file "XXXX/src/tools/emacs/gn.el")

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
    "cd XXXXX && gn refs out/ '"
    (nik/--gn-refs--get-target (buffer-file-name))
    "'")) )

(defun nik/XXXXXX-status ()
  (interactive)
  (magit-status "XXXXXX"))

(defun nik/--close-process-on-success (process event)
  (when (and
         (equal event "finished\n")
         (equal (process-exit-status process) 0))
    (kill-buffer (process-buffer process))))

(defun nik/gclient-sync ()
  (interactive)
  (let ((process (start-process-shell-command
                  "gclient sync"
                  "*gclient sync*"
                  "cd XXXXXX && XXXXXX/nikitools/is-gclient-synced || gclient sync")))
    (with-current-buffer (process-buffer process)
      (display-buffer (current-buffer))
      (shell-mode)
      (set-process-filter process 'comint-output-filter))
    (set-process-sentinel process #'nik/--close-process-on-success)))

(defun nik/gclient-sync-light ()
  (interactive)
  (let ((process (start-process-shell-command
                  "gclient sync"
                  "*gclient sync*"
                  "cd XXXX && XXXX/light_checkout.py")))
    (with-current-buffer (process-buffer process)
      (display-buffer (current-buffer))
      (shell-mode)
      (set-process-filter process 'comint-output-filter))
    (set-process-sentinel process #'nik/--close-process-on-success)))

(defun nik/XXXXXX-export-patches ()
  (interactive)
  (let ((process (start-process-shell-command
                  "export patches"
                  "*export patches*"
                  "cd XXXX && git commit --allow-empty -am . && XXXX/patches_export_all.py")))
    (with-current-buffer (process-buffer process)
      (display-buffer (current-buffer))
      (shell-mode)
      (set-process-filter process 'comint-output-filter))
    (set-process-sentinel process #'nik/--close-process-on-success)))

(defun nik/XXXXXX-reapply-by-commit ()
  (interactive)
  (let ((process (start-process-shell-command
                  "reapply patches"
                  "*reapply patches*"
                  "cd XXXXXX && XXXXXXX/nikitools/patches_reapply_by_commit_experimental.py --repo-dir .. --patch-dir patches")))
    (with-current-buffer (process-buffer process)
      (display-buffer (current-buffer))
      (shell-mode)
      (set-process-filter process 'comint-output-filter))
    (set-process-sentinel process #'nik/--close-process-on-success)))
(defun nik/XXXXXX-reexport-by-commit ()
  (interactive)
  (let ((process (start-process-shell-command
                  "reexport patches"
                  "*reexport patches*"
                  "cd XXXXXX && XXXX/nikitools/patches_reexport_by_commit_experimental.py --repo-dir .. --patch-dir patches")))
    (with-current-buffer (process-buffer process)
      (display-buffer (current-buffer))
      (shell-mode)
      (set-process-filter process 'comint-output-filter))
    (set-process-sentinel process #'nik/--close-process-on-success)))

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
  (let ((root "XXXXXXXX/src"))
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
                     "/XXXXXX/patches/"
                     (replace-regexp-in-string "/" "-" file)
                     ".patch")))
        (find-file-existing patch)))))

(defun nik/XXXXXX-copyright ()
  (interactive)
  (insert 
   "// Copyright " (format-time-string "%Y") " bla\n"
   ))

(defun nik/XXXXXX-header-guard ()
  (interactive)
  (let* ((root "XXXXXX/XXXXXX_src")
         (file (file-relative-name (buffer-file-name) root))
         (upcased-file (upcase file))
         (replaced-file (replace-regexp-in-string "\\.\\|/" "_" upcased-file))
         (guard (concat replaced-file "_")))
    (nik/XXXXXX-copyright)
    (insert
     "\n"
     "#ifndef " guard "\n"
     "#define " guard "\n"
     "\n"
     "#endif  // " guard)
    (previous-line)))

(defun nik/XXXXXX-include ()
  (interactive)
  (let* ((root "XXXXX/src/")
         (file (file-relative-name (buffer-file-name) root))
         (header (file-name-with-extension file ".h")))
    (nik/XXXXXX-copyright)
    (insert
     "\n"
     "#include \"" header "\"\n")))

(defun nik/ifdef-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (open-line 1)
    (insert "#if !BUILDFLAG(XXXX)")))
(defun nik/ifdef-below ()
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)
    (forward-line)
    (insert "#endif")))

(defun nik/insert-commit-info ()
  (interactive)
  (let ((commit (current-word)))
    (beginning-of-line)
    (insert "https://crrev.com/")
    (beginning-of-line)
    (open-line 1)
    (insert
     (shell-command-to-string
      (format "cd XXXXXX && git show -s --format=reference %s" commit)))
    (previous-line)
    (fill-paragraph)
    ))

(general-define-key
 :keymaps 'nik/spc
 :prefix "i"
 "r" #'nik/gn-refs
 "a" #'nik/gn-refs-XXXX
 "g" #'nik/XXXXXX-status
 "c" #'nik/XXXXXX-status
 "p" #'nik/goto-patch-dwim
 "S" #'nik/gclient-sync
 "L" #'nik/gclient-sync-light
 "E" #'nik/XXXXXX-export-patches

 "k" #'nik/ifdef-above
 "j" #'nik/ifdef-below

 "<" #'nik/XXXXXX-reapply-by-commit
 ">" #'nik/XXXXXX-reexport-by-commit
 )
