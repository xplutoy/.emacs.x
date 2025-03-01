;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-11-26 18:12:17
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(setopt warning-minimum-level :error)
(setopt gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook (lambda ()
				(message "")
				(setopt gc-cons-threshold (* 8 1024 1024))))

(let ((my-frame-alist '((font . "Iosevka-15")
			(width . 80) (height . 40)
			(menu-bar-lines . nil)
			(tool-bar-lines . nil)
			(vertical-scroll-bars . nil))))
  (dolist (lst my-frame-alist)
    (add-to-list 'default-frame-alist lst)))

(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(advice-add #'display-startup-echo-area-message :override #'ignore)

;;; early-init.el ends here
