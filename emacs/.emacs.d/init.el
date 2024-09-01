;; init.el -*- mode: lisp-interaction; lexical-binding: t; -*-

(defvar nik/cache
  (expand-file-name ".cache" user-emacs-directory))
(defun nik/cache (path)
  (concat (file-name-as-directory nik/cache) path))

;;; personal settings pre
(defvar nik/light-mode nil)
(load (concat
       (file-name-directory (file-truename load-file-name))
       "../../private/emacs/personal-pre.el") t)

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
  "Sexp providing regexp for finding `use-package' forms in user files.
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
 "DEL" #'evil-ex-nohighlight
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
 "r" #'revert-buffer-quick
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
  (delete 'diff-mode evil-collection-mode-list)
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

(use-package avy
  :general
  (:states 'motion
           "C-;" #'avy-goto-char-timer
           "s" #'avy-goto-char-timer)
  (:states 'normal
           "s" #'avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.3))

(use-package company
  :demand t
  :general
  (:keymaps 'company-active-map
   "C-l" #'company-complete-selection
   "<down>" nil
   "<up>" nil
   "TAB" nil
   "<tab>" nil
   "RET" nil
   "<return>" nil
   "C-w" nil
   "C-d" nil)
  :config
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case t)
  (setq company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  (global-company-mode +1))

(use-package company-statistics
  :config
  (company-statistics-mode))

;;; formatting
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)

(defun nik/plist-set-path (plist props val)
  (let ((prop (car props))
        (rest (cdr props)))
    (if rest
        (plist-put
         plist prop (nik/plist-set-path
                     (plist-get plist prop) rest val))
      (plist-put plist prop val))))

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
   "o" #'lsp-clangd-find-other-file)
  (:keymaps 'nik/spc
   "\"" #'lsp-find-implementation
   "'" #'lsp-find-type-definition
   )
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
                                  rust-mode
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
  (setq lsp-auto-execute-action nil)

  (advice-add
   #'lsp-rust-analyzer--make-init-options
   :filter-return
   (lambda (init-options)
     (nik/plist-set-path
      init-options '(:hover :memoryLayout :enable) :json-false)))

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

(use-package lsp-ui
  :general
  (:keymaps 'nik/spc
   :prefix "c"
   "I" #'lsp-ui-imenu
   "d" #'lsp-ui-doc-show)
  :config
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-imenu-index-function #'lsp-imenu-create-categorized-index)
  )

(use-package lsp-pyright)

(use-package flycheck
  :config
  (add-to-list 'flycheck-check-syntax-automatically 'idle-buffer-switch))

(use-package yasnippet
  :demand t
  :config
  (yas-global-mode +1))

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
  (setq magit-diff-refine-hunk nil
	;; Buggy implementation in magit-extras. Check later if it's fixed.
	magit-bind-magit-project-status nil
        magit-diff-extra-stat-arguments '("--stat-width" "1000")
        magit-log-margin-show-committer-date t)
  (delete #'magit-blame-maybe-update-revision-buffer magit-blame-goto-chunk-hook))

(use-package magit-tbdiff
  :after magit)

(use-package git-link
  :general
  (:keymaps 'nik/spc
   :prefix "g"
   "l" #'git-link)
  :config
  (defun nik/git-link-chromium (hostname dirname filename branch commit start end)
    (format "https://source.chromium.org/chromium/chromium/src/+/main:%s;l=%s"
            filename
            start))
  (add-to-list 'git-link-remote-alist
               '("chromium\\.googlesource\\.com" nik/git-link-chromium))
  (setq git-link-use-commit t))

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
      scroll-conservatively 0)

;;; Disable bell
(setq ring-bell-function #'ignore)

(use-feature imenu
  :init
  (with-eval-after-load 'lisp-mode
    (add-to-list 'lisp-imenu-generic-expression
		 (list "Packages" "^;;;\\s-*\\(.*\\)" 1)))
  :config
  (setq imenu-max-item-length nil))

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
  (:keymaps 'nik/spc
   "*" #'nik/consult-line-symbol-at-point)

  :config/el-patch
  ;; Work around fd searching full path: https://github.com/sharkdp/fd/issues/839
  (defun consult--fd-make-builder (paths)
    "Build find command line, finding across PATHS."
    (let ((cmd (consult--build-args consult-fd-args)))
      (lambda (input)
        (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                     (flags (append cmd opts))
                     (ignore-case
                      (and (not (or (member "-s" flags) (member "--case-sensitive" flags)))
                           (or (member "-i" flags) (member "--ignore-case" flags)
                               (let (case-fold-search)
                                 ;; Case insensitive if there are no uppercase letters
                                 (not (string-match-p "[[:upper:]]" arg)))))))
          (if (or (member "-F" flags) (member "--fixed-strings" flags))
              (cons (append cmd (list arg) opts paths)
                    (apply-partially #'consult--highlight-regexps
                                     (list (regexp-quote arg)) ignore-case))
            (pcase-let ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'pcre ignore-case)))
              (when re
                (cons (append cmd
                              (mapcan (lambda (x) `("--and" (el-patch-swap ,x ,(concat default-directory ".*" x)))) re)
                              opts
                              (mapcan (lambda (x) `("--search-path" ,x)) paths))
                      hl))))))))

  :config
  (setq consult-fd-args
        '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
          "--full-path --color=never --hidden --type file"))

  (setq consult-ripgrep-args
        (concat consult-ripgrep-args " --hidden"))

  ;; Use aggressive async refresh
  (setq consult-async-min-input 1
        consult-async-input-debounce 0.02
        consult-async-input-throttle 0.02
        consult-async-refresh-delay 0.02
        )

  (setq consult-narrow-key ">"
        consult-preview-key "C-;")

  (defun nik/consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))

  ;; Preview in-buffer stuff
  (consult-customize
   consult-line consult-imenu
   nik/consult-line-symbol-at-point
   :preview-key 'any)

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))

  (setq project-switch-commands
        '((consult-fd "Find file" ?f)
          (consult-ripgrep "Grep dir" ?d)
          (magit-project-status "Git" ?g)))
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
    "Run `magit-status' on repo containing the embark target."
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
   "f" #'project-find-file
   "k" #'project-kill-buffers
   "p" #'project-switch-project)
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
     (project-root project))
   default-directory))

(use-feature dired
  :config
  (setq dired-dwim-target t)
  (setq dired-kill-when-opening-new-dired-buffer t))

(use-feature files
  :init
  (defvar nik/external-local-variables '())

  :config
  (defun nik/add-external-local-variables ()
    (let ((file-name (or (buffer-file-name)
                         (expand-file-name default-directory))))
      (dolist (elem nik/external-local-variables)
        (let ((prefix (car elem))
              (vars-per-mode (cdr elem)))
          (when (string-prefix-p prefix file-name)
            (dolist (elem vars-per-mode)
              (let ((mode (car elem))
                    (vars (cdr elem)))
                (when (or (not mode)
                          (derived-mode-p mode))
                  (dolist (elem vars)
                    (push elem file-local-variables-alist))))))))))

  (advice-add 'hack-local-variables-apply
           :before #'nik/add-external-local-variables))

(use-feature tab-bar
  :general
  ("C-<next>" #'nik/tab-bar-next-tab-or-create
   "C-<prior>" #'tab-bar-switch-to-prev-tab)
  :config
  (defun nik/tab-bar-next-tab-or-create ()
    (interactive)
    (let* ((tabs (funcall tab-bar-tabs-function))
           (index (or (tab-bar--current-tab-index tabs) 0))
           (current-tab-last-p (= index (1- (length tabs)))))
      (if current-tab-last-p
          (tab-bar-new-tab)
        (tab-bar-switch-to-next-tab)))))

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
  :hook (org-mode . turn-on-auto-fill)
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
  (setq doom-gruvbox-brighter-comments t)
  (setq doom-gruvbox-light-brighter-comments t)
  (if nik/light-mode
      (load-theme 'doom-gruvbox-light t)
    (load-theme 'doom-gruvbox t))
  (set-face-attribute
   'tab-bar-tab nil
   :foreground "#fbf1c7" :background "#66542a"
   :weight 'bold))

(use-package google-c-style
  :hook (c-mode-common . google-set-c-style))

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
  (setq display-line-numbers-type 'visual
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

(use-feature so-long
  :demand t
  :config
  (global-so-long-mode +1))

(use-feature compile
  :general
  (:keymaps 'global
   "<f4>" #'nik/recompile)
  :config
  (evil-define-command nik/recompile ()
    :repeat nil
    (interactive)
    (evil-normal-state)
    (save-some-buffers 'no-confirm)
    (recompile))
  )

(use-package haskell-mode)
(use-package lsp-haskell
  :demand t)

(use-package markdown-mode
  :general
  (:keymaps 'markdown-mode-map
   :states 'normal
   "RET" nil)
  )

(use-package yaml-mode)

(use-package json-mode)

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

(use-package svelte-mode)

(use-package jenkinsfile-mode)

(use-package csharp-mode)

(use-package rust-mode
  :config
  (setq lsp-rust-analyzer-lens-enable nil
        ))

(use-package ahk-mode)

;;; mojom
(add-to-list 'auto-mode-alist '("\\.mojom$" . java-mode))

;;; frame config

(add-to-list 'default-frame-alist '(font . "M PLUS 1 Code-11:weight=normal"))
(add-to-list 'default-frame-alist '(undecorated . t))
(setq frame-title-format
      '(multiple-frames
        ("" invocation-name "@" system-name " -- %b")
        ("" invocation-name "@" system-name)))

(setq inhibit-startup-screen t)

;; Cleanup the frame UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

(column-number-mode +1)

;; Maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Don't split vertically
(setq split-height-threshold nil)

(setq use-short-answers t)

(server-start)

;;; Window management
(defun nik/quit-other-window (&optional kill)
  (interactive)
  (quit-window kill (next-window)))
(general-define-key "<f7>" #'nik/quit-other-window)

;;; Enable disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; personal settings post
(load (concat
       (file-name-directory (file-truename load-file-name))
       "../../private/emacs/personal-post.el") t)

;; Should be last
(use-package gcmh
  :demand t
  :config
  (gcmh-mode +1))
