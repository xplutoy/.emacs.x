;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-11-26 18:12:17
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(setq gc-cons-threshold (* 10 1024 1024))

(add-to-list 'default-frame-alist '(width  . 80))
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(menu-bar-lines . nil))
(add-to-list 'default-frame-alist '(tool-bar-lines . nil))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(add-to-list 'default-frame-alist '(font . "Iosevka-15"))

(advice-add #'display-startup-screen :override #'ignore)
(advice-add #'display-startup-echo-area-message :override #'ignore)

(provide 'early-init)
;;; early-init.el ends here



