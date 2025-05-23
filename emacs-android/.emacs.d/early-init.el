;;; early-init.el -*- lexical-binding: t; -*-

(when (string-equal system-type "android")
  ;; Add Termux binaries to PATH environment
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath))
    (setq exec-path (append exec-path (list termuxpath)))))

;; Using straight.el, disable package.el
(setq package-enable-at-startup nil)
