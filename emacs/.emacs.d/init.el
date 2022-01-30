;; early-init.el -*- mode: lisp-interaction; lexical-binding: t; -*-

;; TODO Deal with native compilation once Emacs 28 is out.

(defvar nik/cache
  (expand-file-name "cache" user-emacs-directory))
(defun nik/cache (path)
  (concat (file-name-as-directory nik/cache) path))

;; Don't create backup, autosave and lock files
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil)

;;; straight
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

;;; use-package
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

;; Create leader SPC keymap
(define-prefix-command 'nik/spc)
(general-define-key
 :states '(normal visual motion)
 :keymaps 'override
 "SPC" nik/spc)

(defun nik/test ()
  "Used when testing keybindings"
  (interactive)
  (message "Test command %s" (format-time-string "%FT%T")))

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

(defun nik/copy-file-path ()
  "Copy the full path of the current buffer's file."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "%s" filepath))))

;;; General shortcuts/keymaps
(general-define-key
 :keymaps 'nik/spc
 "f" #'find-file
 "F" #'find-file-other-window
 "e" #'switch-to-buffer
 "E" #'switch-to-buffer-other-window
 )

(defun nik/find-init ()
  (interactive)
  (find-file
   (expand-file-name "init.el" user-emacs-directory))
  (consult-imenu))

(general-define-key
 :keymaps 'nik/spc
 :prefix "b"
 "r" #'revert-buffer
 "c" #'nik/copy-file-path
 "i" #'nik/find-init
 "k" #'kill-buffer
 )

(general-define-key
 :keymaps 'nik/spc
 :prefix "j"
 "i" #'imenu
 )

(use-package evil-collection
  :demand t
  :after evil
  :config
  (evil-collection-init))

(use-package undo-tree
  :hook (evil-local-mode . turn-on-undo-tree-mode))

(use-package evil-surround
  :general
  (:states 'visual
   "s" #'evil-surround-region)
  ;; Specify operator as well to support lazy loading
  (:states 'operator
   "s" #'evil-surround-edit)
  :config
  (global-evil-surround-mode +1))

(use-package company
  :demand t
  :general
  (:keymaps 'company-active-map
   "C-w" nil
   "C-d" nil)
  :config
  (global-company-mode +1)
  (company-tng-mode +1))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Show additional search match info
(use-package anzu
  :demand t
  :config
  (global-anzu-mode +1))
(use-package evil-anzu
  :demand t
  :after evil)

(use-package magit
  :general
  (:keymaps 'nik/spc
   :prefix "g"
   "g" #'magit-status
   "d" #'magit-dispatch
   "b" #'magit-blame-addition
   "f" #'magit-file-dispatch)
  (:keymaps 'magit-diff-section-base-map
   "C-<return>" #'magit-diff-visit-worktree-file-other-window)
  :init
  (setq magit-define-global-key-bindings nil)
  :config
  (setq magit-diff-refine-hunk t))

(use-package transient
  :init
  (setq transient-levels-file (nik/cache "transient/levels.el")
	transient-values-file (nik/cache "transient/values.el")
	transient-history-file (nik/cache "transient/history.el")))

;;; Nicer scrolling
(setq scroll-step 1
      scroll-conservatively 101)

;;; Disable bell
(setq ring-bell-function #'ignore)

(use-feature imenu
  :init
  (with-eval-after-load 'lisp-mode
    (add-to-list 'lisp-imenu-generic-expression
		 (list "Packages" "^;;;\\s-*\\(.*\\)" 1))))

(use-package vertico
  :straight (:files (:defaults "extensions/*.el"))
  :demand t
  :general
  (:keymaps 'vertico-map
   "C-h" #'vertico-directory-delete-word
   "C-j" #'vertico-next
   "C-k" #'vertico-previous
   "C-l" #'vertico-insert
   "M-h" help-map)
  :config
  (setq vertico-count 20)
  (vertico-mode +1))

(use-package orderless
  :demand t
  :config
  (setq orderless-matching-styles '(orderless-regexp orderless-literal)
	orderless-component-separator #'orderless-escapable-split-on-space
	completion-styles '(orderless)))

(use-package consult
  :demand t
  :general
  ([remap apropos]                       #'consult-apropos
   [remap bookmark-jump]                 #'consult-bookmark
   [remap evil-show-marks]               #'consult-mark
   [remap evil-show-jumps]               #'evil-collection-consult-jump-list
   [remap evil-show-registers]           #'consult-register
   [remap goto-line]                     #'consult-goto-line
   [remap imenu]                         #'consult-imenu
   [remap locate]                        #'consult-locate
   [remap load-theme]                    #'consult-theme
   [remap man]                           #'consult-man
   [remap recentf-open-files]            #'consult-recent-file
   [remap switch-to-buffer]              #'consult-buffer
   [remap switch-to-buffer-other-window] #'consult-buffer-other-window
   [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
   [remap yank-pop]                      #'consult-yank-pop
   [remap isearch-forward]               #'consult-line)
  :config
  (setq consult-narrow-key ">"))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode))

(use-feature dired
  :config
  (setq dired-dwim-target t))

(add-to-list 'default-frame-alist '(font . "M PLUS 1 Code-11"))

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; Set brighter comments
  (setq doom-gruvbox-brighter-comments t)
  (load-theme 'doom-gruvbox t))

;; Collapses widely used minor modes
(use-package minions
  :demand t
  :config
  (minions-mode +1))

(use-feature display-line-numbers
  :demand t
  :config
  (setq display-line-numbers-type 'relative
	display-line-numbers-current-absolute nil)
  (global-display-line-numbers-mode +1))

(use-feature savehist
  :demand t
  :config
  (setq savehist-file (nik/cache "history"))
  (savehist-mode +1))

(use-feature recentf
  :demand t
  :config
  (setq recentf-save-file (nik/cache "recentf")
        recentf-max-saved-items 10000)
  (recentf-mode +1))

(use-feature bookmark
  :config
  (setq bookmark-file (nik/cache "bookmarks")))

;;; frame config

(setq inhibit-startup-screen t)

;; Cleanup the frame UI
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Don't split vertically
(setq split-height-threshold nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(server-start)

;; Should be last
(use-package gcmh
  :demand t
  :config
  (gcmh-mode +1))
