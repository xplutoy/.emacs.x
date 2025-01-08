;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-11-26 18:12:17
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(setopt debug-on-error t)
(setopt system-time-locale "C")
(setopt gc-cons-threshold most-positive-fixnum)
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'default-frame-alist '(width  . 80))
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(menu-bar-lines . nil))
(add-to-list 'default-frame-alist '(tool-bar-lines . nil))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(add-to-list 'default-frame-alist '(font . "Iosevka-15"))

(add-hook 'after-init-hook (lambda ()
			     (load custom-file t t)
			     (setopt debug-on-error nil)
			     (setopt gc-cons-threshold (* 16 1024 1024))))

(advice-add #'display-startup-screen :override #'ignore)
(advice-add #'display-startup-echo-area-message :override #'ignore)

(provide 'early-init)
;;; early-init.el ends here
