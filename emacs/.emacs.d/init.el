;; init.el -*- mode: lisp-interaction; lexical-binding: t; -*-

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
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; el-patch
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

(use-package which-key
  :demand t
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-add-column-padding 8)
  (which-key-mode +1))

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
  (setq evil-want-keybinding nil
	evil-want-Y-yank-to-eol t
	evil-want-C-u-delete t

        ;; TODO Emacs 28 changes stuff?
        evil-undo-system 'undo-tree

        evil-search-module 'evil-search
        evil-symbol-word-search t)

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
        evil-visual-state-cursor 'hollow
        evil-emacs-state-cursor 'hbar
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        ;; End/beginning of line is not an error
        evil-kbd-macro-suppress-motion-error t
        evil-want-visual-char-semi-exclusive t
        )
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode +1))

;;; General shortcuts/keymaps
(general-define-key
 :keymaps 'nik/spc
 "f" #'find-file
 "F" #'find-file-other-window
 "e" #'switch-to-buffer
 "E" #'switch-to-buffer-other-window
 "h" help-map
 )

(defun nik/find-init ()
  (interactive)
  (find-file
   (expand-file-name "init.el" user-emacs-directory))
  (consult-imenu))

(defun nik/copy-file-path ()
  "Copy the full path of the current buffer's file."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "%s" filepath))))

(defun nik/copy-file-path-and-line ()
  "Copy the full path of the current buffer's file."
  (interactive)
  (let* ((filepath (if (equal major-mode 'dired-mode)
		       default-directory
		     (buffer-file-name)))
	 (line (number-to-string (line-number-at-pos)))
	 (out (concat filepath ":" line)))
    (when filepath
      (kill-new out)
      (message "%s" out))))

(general-define-key
 :keymaps 'nik/spc
 :prefix "b"
 "r" #'revert-buffer
 "c" #'nik/copy-file-path
 "l" #'nik/copy-file-path-and-line
 "i" #'nik/find-init
 "k" #'kill-buffer
 )

(general-define-key
 :keymaps 'nik/spc
 :prefix "j"
 "i" #'imenu
 "m" #'bookmark-jump
 )

;; Since this package overrides the built-in xref, load it early
(use-package xref
  :general
  (:states 'normal
   "M-]" #'xref-find-references)
  :commands (xref-find-definitions)
  :config
  (setq xref-prompt-for-identifier nil)
  )

(use-package evil-collection
  :demand t
  :after evil
  :config
  (evil-collection-init))

(use-package undo-tree
  :hook (evil-local-mode . turn-on-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil))

(use-package evil-surround
  :general
  (:states 'visual
   "s" #'evil-surround-region)
  ;; Specify operator as well to support lazy loading
  (:states 'operator
   "s" #'evil-surround-edit)
  :config
  (global-evil-surround-mode +1))

(use-package evil-nerd-commenter
  :after evil
  :general
  (:states '(normal, visual) "gc" #'evilnc-comment-operator))

(use-feature elec-pair
  :demand t
  :config
  (electric-pair-mode +1))


(use-package company
  :demand t
  :general
  (:keymaps 'company-active-map
   "C-w" nil
   "C-d" nil)
  :config
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case t)
  (global-company-mode +1)
  (company-tng-mode +1))

(use-package company-box
  :hook (company-mode . company-box-mode))

;;; formatting
(setq-default indent-tabs-mode nil)

(use-package lsp-mode
  :general
  ;; Set the lsp prefix key
  (:keymaps 'lsp-mode-map
   "C-c l" '(:keymap lsp-command-map :which-key "lsp"))
  (:keymaps 'nik/spc
   :prefix "c"
   "a" #'lsp-execute-code-action
   "r" #'lsp-rename
   "R" #'lsp-workspace-restart
   "I" #'lsp-ui-imenu
   "o" #'lsp-clangd-find-other-file)
  (:keymaps 'nik/spc
   "\"" #'lsp-find-implementation)
  :init
  ;; Set a high read output max value for handling large language server responses
  (setq read-process-output-max (* 10 1024 1024))
  ;; Set a short delay for refreshing state after moving the cursor
  (setq lsp-idle-delay 0.2)
  ;; Enable which-key help on the lsp prefix key
  (setq lsp-keymap-prefix "C-c l")
  ;; Enable for the following modes
  (setq nik/lsp-enable-for-modes '(c-mode
                                  c++-mode
                                  objc-mode
                                  haskell-mode
                                  haskell-literate-mode
                                  go-mode
                                  csharp-mode
                                  ;; java-mode
                                  (python-mode (lambda () (require 'lsp-pyright)))
                                  js2-mode
                                  typescript-mode
                                  svelte-mode
                                  groovy-mode
                                  web-mode
                                  json-mode
                                  yaml-mode
                                  dockerfile-mode
                                  terraform-mode
                                  cmake-mode
                                  sh-mode))

  (defun nik/maybe-enable-lsp (lsp-config)
    "If mode in LSP-CONFIG is equal to the current major-mode,
run the attached function (if exists) and enable lsp"
    (pcase lsp-config
      (`(,(pred (equal major-mode)) ,func) (funcall func) (lsp) t)
      ((pred (equal major-mode)) (lsp) t)))

  ;; Kill language server after the last associated buffer was closed
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-session-file (nik/cache "lsp-session-v1"))
  (setq lsp-eslint-library-choices-file (nik/cache ".lsp-eslint-choices"))
  ;; Force lsp mode to forget the workspace folders for multi root servers
  ;; so the folders are added on demand
  (advice-add 'lsp :before
              (lambda (&rest _args)
                (eval
                 '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  ;; Enable semantic token highlighting
  (setq lsp-semantic-tokens-enable t)
  ;; Set clangd default parameters
  (setq lsp-clients-clangd-args '("--header-insertion-decorators=0"
                                  "--completion-style=detailed"))
  :hook
  ;; Postpone lsp load for after dir local vars are read
  ;; Do not load lsp if dir local vars are not enabled (e.g. on preview)
  (hack-local-variables . (lambda ()
                            (when enable-dir-local-variables
                              (seq-find #'nik/maybe-enable-lsp
                                        nik/lsp-enable-for-modes))))

  ;; Enable which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package lsp-pyright)

(use-package yasnippet
  ;; Needed for typescript mode because language server doesn't
  ;; respect the fact that snippets are disabled. See
  ;; https://github.com/typescript-language-server/typescript-language-server/issues/130
  ;; and
  ;; https://github.com/emacs-lsp/lsp-mode/blob/6327359f3b5e19aeaa1c9ee6bd9b80b51f95f843/lsp-completion.el#L584
  :hook (typescript-mode . yas-minor-mode))

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
  (setq magit-diff-refine-hunk t
	;; Buggy implementation in magit-extras. Check later if it's fixed.
	magit-bind-magit-project-status nil))

(use-package transient
  :init
  (setq transient-levels-file (nik/cache "transient/levels.el")
	transient-values-file (nik/cache "transient/values.el")
	transient-history-file (nik/cache "transient/history.el")))

(use-feature vc-hooks
  :config
  (setq vc-follow-symlinks t))

;; Use "dnf install libvterm-devel"
(use-package vterm
  :general
  ("<f8>" #'nik/vterm-project)
  :init
  ;; Set a low response delay
  (setq vterm-timer-delay 0.07)
  (setq vterm-max-scrollback 5000)
  :config/el-patch
  (defun vterm--internal (pop-to-buf-fun &optional arg)
    (cl-assert vterm-buffer-name)
    (let ((buf (cond ((numberp arg)
                      (get-buffer-create (format "%s<%d>"
                                                 vterm-buffer-name
                                                 arg)))
                     ((stringp arg) ((el-patch-swap generate-new-buffer get-buffer-create) arg))
                     (arg (generate-new-buffer vterm-buffer-name))
                     (t
                      (get-buffer-create vterm-buffer-name)))))
      (cl-assert (and buf (buffer-live-p buf)))
      (funcall pop-to-buf-fun buf)
      (with-current-buffer buf
        (unless (derived-mode-p 'vterm-mode)
          (vterm-mode)))
      buf))
  :config
  (defun nik/vterm-project (&optional arg)
    (interactive "P")
    (let* ((default-directory (nik/project-root))
           (vterm-name (format "%s[%s]%s"
                               vterm-buffer-name
                               default-directory
                               (if (numberp arg) (format "<%d>" arg) ""))))
      (vterm vterm-name)))
  )

;;; custom
(setq custom-file (nik/cache "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

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
  (:keymaps 'nik/spc
   :prefix "s"
   "d" #'consult-ripgrep
   "D" (lambda () (interactive) (consult-ripgrep t))
   "f" #'consult-fd
   "F" (lambda () (interactive) (consult-fd t)))
  :config
  (setq consult-narrow-key ">"
	consult-preview-key (kbd "C-;"))

  ;; Preview in-buffer stuff
  (consult-customize
   consult-line consult-imenu
   :preview-key 'any)

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))

  ;; Based on code from consult wiki:
  ;; https://github.com/minad/consult/wiki#find-files-using-fd
  (defvar consult--fd-command nil)
  (defun consult--fd-builder (input)
    (unless consult--fd-command
      (setq consult--fd-command
            (if (eq 0 (call-process-shell-command "fdfind"))
                "fdfind"
              "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended t)))
      (when re
        (list :command (append
                        (list consult--fd-command
                              "--color=never" "-i" "-p" "-H" "-t" "f"
                              (consult--join-regexps re 'extended))
                        opts)
              :highlight hl))))

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
	   (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))
  (setq project-switch-commands
        '((consult-find "Find file" ?f)
          (consult-ripgrep "Grep dir" ?d)
	  (magit-status "Git" ?g)))
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  )

(use-package consult-lsp
  :general
  (:keymaps 'nik/spc
   :prefix "c"
   "s" #'consult-lsp-symbols))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode))

(use-package embark
  :general
  ("M-z" #'embark-act)
  (:keymaps 'embark-file-map
   "g" #'nik/embark-magit-status)
  :config
  (defun nik/embark-magit-status (file)
    "Run `magit-status` on repo containing the embark target."
    (interactive "GFile: ")
    (magit-status (locate-dominating-file file ".git")))
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :demand t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package project
  :demand t
  :general
  (:keymaps 'nik/spc
   :prefix "p"
   "w" #'nik/project-save
   "d" #'project-dired
   "k" #'project-kill-buffers
   "p" #'project-switch-project)
  :config/el-patch
  (defun project-try-vc (dir)
    (let* ((backend (ignore-errors (vc-responsible-backend dir)))
           (root
            (pcase backend
              ('Git
               ;; Don't stop at submodule boundary.
               (or (vc-file-getprop dir 'project-git-root)
                   (vc-file-setprop dir 'project-git-root
                                    (vc-find-root dir (el-patch-swap ".git/" ".git")))))
              ('nil nil)
              (_ (ignore-errors (vc-call-backend backend 'root dir))))))
      (and root (cons 'vc root))))
  :config
  (defun nik/project-save ()
    "Save the current project to the persistent project list."
    (interactive)
    (message "Project saved: %s" (cdr (project-current t))))
  (setq project-list-file (nik/cache "projects")))

(defun nik/project-root ()
  "The project root is used if found by project, with the default
directory as a fall back."
  (or
   (when-let ((project (project-current)))
     (car (last (car (project-roots project)))))
   default-directory))

(use-feature dired
  :config
  (setq dired-dwim-target t))

(use-package helpful
  :general
  ([remap describe-function] #'helpful-callable
   [remap describe-variable] #'helpful-variable
   [remap describe-key]      #'helpful-key
   [remap describe-symbol]   #'helpful-symbol)
  (:keymaps 'nik/spc
   :prefix "h"
   "h" #'helpful-at-point))

(use-package org
  :general
  (:keymaps 'nik/spc
   :prefix "n"
   "l" #'org-store-link
   "o" #'org-open-at-point
   "a" #'org-agenda)
  (:keymaps 'org-mode-map
   :states 'normal
   "TAB" #'org-cycle)
  :config
  (require 'org-tempo)
  (setq org-startup-indented t)
  (setq org-cycle-separator-lines 1)
  (setq org-edit-src-content-indentation 0)
  (setq org-directory "~/wiki"))

(use-package org-roam
  :general
  (:keymaps 'nik/spc
   :prefix "n"
   "b" #'org-roam-buffer-toggle
   "g" #'org-roam-graph
   "i" #'org-roam-node-insert
   "n" #'org-roam-node-find
   "t" #'org-roam-tag-add)
  :custom
  (org-roam-directory
   (concat (file-name-as-directory org-directory) "org-roam"))
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-db-location (nik/cache "org-roam.db"))
  (org-roam-db-autosync-mode))

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; Set brighter comments
  (setq doom-gruvbox-brighter-comments t)
  (load-theme 'doom-gruvbox t))

(use-feature paren
  :demand t
  :config
  (setq show-paren-delay 0)
  (show-paren-mode +1))

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

(use-package haskell-mode)
(use-package lsp-haskell
  :demand t)

(use-package yaml-mode)

(use-package json-mode)

(use-package typescript-mode)

(use-package svelte-mode)

(use-package jenkinsfile-mode)

;;; mojom
(add-to-list 'auto-mode-alist '("\\.mojom$" . java-mode))

;;; frame config

(add-to-list 'default-frame-alist '(font . "M PLUS 1 Code-11"))
(setq frame-title-format
      '(multiple-frames
        ("" invocation-name "@" system-name " -- %b")
        ("" invocation-name "@" system-name)))

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

;;; Enable disabled commands
(put 'narrow-to-region 'disabled nil)

;;; personal settings
(load (concat (file-name-directory load-file-name) "personal.el") t)

;; Should be last
(use-package gcmh
  :demand t
  :config
  (gcmh-mode +1))
