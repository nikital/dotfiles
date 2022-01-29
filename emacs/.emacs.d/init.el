;;; early-init.el -*- lexical-binding: t; -*-

;; TODO Deal with native compilation once Emacs 28 is out.

(defvar nik/cache
  (expand-file-name "cache" user-emacs-directory))
(defun nik/cache (path)
  (concat (file-name-as-directory nik/cache) path))

;; straight
(setq straight-base-dir nik/cache)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'el-patch)

;; use-package
(setq use-package-always-defer t
      use-package-enable-imenu-support t
      straight-use-package-by-default t
      )
;; Include `use-feature' in `imenu'
(el-patch-defcustom use-package-form-regexp-eval
  `(concat ,(eval-when-compile
              (concat "^\\s-*("
                      (regexp-opt '("use-package" (el-patch-add "use-feature") "require") t)
                      "\\s-+\\("))
           (or (bound-and-true-p lisp-mode-symbol-regexp)
               "\\(?:\\sw\\|\\s_\\|\\\\.\\)+") "\\)")
  "Sexp providing regexp for finding use-package forms in user files.
This is used by `use-package-jump-to-package-form' and
`use-package-enable-imenu-support'."
  :type 'sexp
  :group 'use-package)
(straight-use-package 'use-package)

;; Inspired by github.com/raxod502/radian
(defmacro use-feature (name &rest args)
  "Like `use-package', but without straight.el integration.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

(use-package general
  :demand t)

(use-package evil
  :demand t
  :init
  ;; Needed for evil-collection
  (setq evil-want-keybinding nil)

  :general
  (:states 'normal
   "RET" #'nik/save)
  (:states 'motion
   "C-j" #'nik/evil-scroll-down
   "C-k" #'nik/evil-scroll-up)
  (:states 'insert
   "C-e" #'end-of-line)

  :config
  (evil-define-command nik/save ()
    :repeat nil
    (interactive)
    (save-some-buffers 'no-confirm))
  (evil-define-command nik/evil-scroll-up ()
    :repeat nil
    :keep-visual t
    (interactive)
    (evil-scroll-line-up 5))
  (evil-define-command nik/evil-scroll-down ()
    :repeat nil
    :keep-visual t
    (interactive)
    (evil-scroll-line-down 5))

  (setq evil-ex-search-vim-style-regexp t
        evil-symbol-word-search t
        evil-visual-state-cursor 'hollow
        evil-emacs-state-cursor 'hbar
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        ;; End/beginning of line is not an error
        evil-kbd-macro-suppress-motion-error t
        ;; TODO Emacs 28 changes stuff?
        evil-undo-system 'undo-tree
        evil-want-C-u-delete t
        evil-want-Y-yank-to-eol t
        evil-search-module 'evil-search
        evil-want-visual-char-semi-exclusive t
        )

  (evil-mode +1))

(use-package undo-tree
  :hook (evil-local-mode . turn-on-undo-tree-mode))

;; Don't create backup, autosave and lock files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Disable bell
(setq ring-bell-function #'ignore)

(use-feature dired
  :config
  (setq dired-dwim-target t))

;; Should be last
(use-package gcmh
  :demand t
  :config
  (gcmh-mode +1))
