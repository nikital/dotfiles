;; init.el -*- mode: lisp-interaction; lexical-binding: t; -*-

(defvar nik/cache
  (expand-file-name ".cache" user-emacs-directory))
(defun nik/cache (path)
  (concat (file-name-as-directory nik/cache) path))

(defvar nik/gtd "gtd")
(defvar nik/cook "cook")
(load (concat
       (file-name-directory (file-truename load-file-name))
       "../../private/emacs/android-pre.el") t)

(setq touch-screen-display-keyboard t)

(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil)

(setq byte-compile-warnings nil)

(set-face-attribute 'default nil :height 130)

(setq fancy-startup-text
      `((:face (variable-pitch font-lock-comment-face)
               :link ("GTD"
	              ,(lambda (_button)
                         (find-file nik/gtd)
                         (org-show-todo-tree nil))
	              "Open GTD")
               "\n")
        (:face (variable-pitch font-lock-comment-face)
               :link ("Cooking"
	              ,(lambda (_button)
                         (find-file nik/cook))
	              "Open Cooking")
               "\n")))

(tool-bar-add-item "help" #'org-cycle 'org-cycle)
