;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-11-26 18:12:17
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(add-to-list 'default-frame-alist '(font . "Iosevka-14"))
(add-to-list 'default-frame-alist '(menu-bar-lines . nil))
(add-to-list 'default-frame-alist '(tool-bar-lines . nil))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(advice-add #'display-startup-screen :override #'ignore)
(advice-add #'display-startup-echo-area-message :override #'ignore)

(provide 'early-init)
;;; early-init.el ends here



