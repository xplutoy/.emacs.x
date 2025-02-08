;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-11-26 18:12:17
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(setopt debug-on-error t)
(setopt gc-cons-threshold (* 16 1024 1024))

(add-to-list 'default-frame-alist '(font . "Iosevka-15"))

(add-to-list 'default-frame-alist '(width  . 80))
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(menu-bar-lines . nil))
(add-to-list 'default-frame-alist '(tool-bar-lines . nil))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(advice-add #'display-startup-echo-area-message :override #'ignore)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
