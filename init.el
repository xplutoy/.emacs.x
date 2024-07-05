;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-07-05 11:18:22
;; Modified: <2024-07-05 12:01:16 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar yx/org-dir "~/org-notes/")

(set-language-environment "UTF-8")

(with-eval-after-load 'package
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t))

(setopt fill-column 89)

(setopt user-mail-address "yangxue.cs@foxmail.com")

(setopt frame-title-format '("%b - GNU Emacs"))

(setopt word-wrap t)

(setopt word-wrap-by-category t)

(setopt truncate-lines t)

(setopt tab-always-indent 'complete)

(setopt sentence-end-double-space nil)

(setopt use-short-answers t)

(setopt system-time-locale "C")

(setopt set-mark-command-repeat-pop t)

(setopt enable-recursive-minibuffers t)

(setopt cursor-in-non-selected-windows t)

(setopt find-file-visit-truename t)

(setopt inhibit-splash-screen t)

(setopt inhibit-startup-message t)

(setopt initial-scratch-message nil)

(setopt switch-to-buffer-obey-display-actions t)

(setopt dired-kill-when-opening-new-dired-buffer t)

(setopt winner-boring-buffers-regexp "^\\*")

(setopt epg-pinentry-mode 'loopback)

(setopt epa-file-encrypt-to user-mail-address)

(setopt recentf-auto-cleanup 300)

;; global mirror mode

(winner-mode +1)

(windmove-default-keybindings 'control)

(tool-bar-mode -1)

(scroll-bar-mode -1)

(blink-cursor-mode -1)

(repeat-mode +1)

(savehist-mode +1)

(save-place-mode +1)

(delete-selection-mode +1)

(auto-save-visited-mode +1)

(global-auto-revert-mode +1)

(pixel-scroll-precision-mode +1)

(add-hook 'after-init-hook #'recentf-mode)

(add-hook 'text-mode-hook #'variable-pitch-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Ui
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-loop for font in '("LXGW WenKai" "Microsoft Yahei" "PingFang SC")
	 when (x-list-fonts font)
	 return (set-fontset-font t 'han (font-spec :family font)))

(load-theme 'modus-operandi t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(keymap-global-unset "C-z")

(keymap-global-set "C-z ." #'repeat)
(keymap-global-set "C-z a" #'org-agenda-list)
(keymap-global-set "C-z c" #'org-capture)
(keymap-global-set "C-z l" #'org-store-link)
(keymap-global-set "C-z s" #'scratch-buffer)

(keymap-global-set "C-c d" #'duplicate-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path
	     (expand-file-name "extras" user-emacs-directory))

(require 'init-simple)

(require 'init-base)

(require 'init-org)

(require 'init-writting)

(require 'init-dev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Customization
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(package-selected-packages
   '(olivetti orderless no-littering magit denote embark-consult consult embark corfu vertico))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Courier New" :foundry "outline" :slant normal :weight regular :height 143 :width normal)))))
