;;; early-init.el -*- lexical-binding: t; -*-

;; Disable GC during init, restore later by running `gchm-mode'.
(setq gc-cons-threshold most-positive-fixnum)

;; Using straight.el, disable package.el
(setq package-enable-at-startup nil)
