;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-11-26 18:12:17
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(setopt warning-minimum-level :error)
(setopt system-time-locale "C")
(setopt bidi-inhibit-bpa t)
(setopt inhibit-compacting-font-caches t)
(setopt frame-inhibit-implied-resize t)
(setopt redisplay-skip-fontification-on-input t)
(setopt read-process-output-max (* 8 1024 1024))

(setq-default bidi-display-reordering nil)

(set-language-environment "UTF-8")

;; (defvar yx/en-font "Iosevka")
(defvar yx/en-font "Aporetic Sans Mono")
(defvar yx/cn-font "LXGW WenKai Mono Screen")

(set-face-attribute 'default nil :family yx/en-font :height 160)
(set-face-attribute 'fixed-pitch nil :family yx/en-font :height 1.0)

(let ((my-frame-alist '((width . (text-pixels . 1000))
			(height . (text-pixels . 750))
			(menu-bar-lines . nil)
			(tool-bar-lines . nil)
			(vertical-scroll-bars . nil))))
  (dolist (lst my-frame-alist)
    (add-to-list 'default-frame-alist lst)))

(when (featurep 'ns)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; avoid flashing at startup
(defun yx/avoid-flashing-at-startup ()
  "Avoid flashing at startup via delaying `make-frame-visible'."
  (push '(visibility . nil) initial-frame-alist)
  (add-hook 'emacs-startup-hook #'make-frame-visible))

(yx/avoid-flashing-at-startup)

(advice-add #'display-startup-echo-area-message :override #'ignore)

;;; early-init.el ends here
