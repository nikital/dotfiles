;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Nikita Leshenko"
      user-mail-address "nikita@leshenko.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/wiki/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;; Global maps

(map! "C-s" #'swiper-isearch
      :leader
      :desc "Find file other window" "f F" #'find-file-other-window)

;;; Evil

(defun save-some-buffers-no-confirm ()
  (interactive)
  (save-some-buffers 'no-confirm))

(defun nik/evil-scroll-up ()
  (interactive)
  (evil-scroll-line-up 5))
(defun nik/evil-scroll-down ()
  (interactive)
  (evil-scroll-line-down 5))

(map! :i "C-e" #'end-of-line
      :n "RET" #'save-some-buffers-no-confirm
      :m "C-j" #'nik/evil-scroll-down
      :m "C-k" #'nik/evil-scroll-up
      :n "C-e" #'counsel-switch-buffer
      :n "M-e" #'counsel-switch-buffer-other-window)

(after! evil
  (setq evil-want-C-u-scroll nil)
  (evil-declare-not-repeat 'save-some-buffers-no-confirm))

(after! evil-collection
  (defun nik/unmap-outline-keys ()
    (map! :map outline-mode-map
          :n "C-j" nil
          :n "C-k" nil))
  (advice-add #'evil-collection-outline-setup :after #'nik/unmap-outline-keys))

;; I'm not sure about snipe...
(after! evil-snipe
  (setq evil-snipe-scope 'visible)
  ; Get used to snipe's repeating f
  (map! :m ";" nil
        :m "," nil
        :map evil-snipe-override-mode-map
        :m ";" nil
        :m "," nil
        :map evil-snipe-parent-transient-map
        ";" nil
        "," nil))

;;; Company
(after! company
  (map! :map company-active-map
        "C-w" nil
        "C-j" nil
        "C-k" nil)
  (company-tng-configure-default))
(after! (:and company evil)
  (add-hook 'evil-insert-state-exit-hook #'company-cancel))

;;; Magit
(after! magit
  (map! :map magit-mode-map
        :n "C-j" nil
        :n "C-k" nil))
