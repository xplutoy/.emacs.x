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
(setopt read-process-output-max (* 1024 1024))

(set-language-environment "UTF-8")

(defvar yx/en-font "JetBrains Maple Mono")
(defvar yx/cn-font "JetBrains Maple Mono")

(set-face-attribute 'default nil :family yx/en-font :height 150)
(set-face-attribute 'fixed-pitch nil :family yx/en-font :height 1.0)

(let ((my-frame-alist '((width . 72) (height . 36)
			(menu-bar-lines . nil)
			(tool-bar-lines . nil)
			(vertical-scroll-bars . nil))))
  (dolist (lst my-frame-alist)
    (add-to-list 'default-frame-alist lst)))

(when (featurep 'ns)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(advice-add #'display-startup-echo-area-message :override #'ignore)

;;; early-init.el ends here
