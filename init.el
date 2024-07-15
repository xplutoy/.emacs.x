;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 11:18:22
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar yx/org-dir "~/org-notes/")

(defconst IS-MAC (eq system-type 'darwin))

(defconst IS-WIN (eq system-type 'windows-nt))

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

(setopt require-final-newline t)

(setopt indent-tabs-mode nil)

(setopt tab-always-indent 'complete)

(setopt sentence-end-double-space nil)

(setopt use-dialog-box nil)

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

(setopt split-width-threshold 120)

(setopt custom-file null-device)

(setopt display-buffer-alist
	'(("\\`\\*\\(Compile-Log\\|Org Links\\)\\*\\'"
	   (display-buffer-no-window)
	   (allow-no-window . t))
	  ("\\`\\*\\(Org Select\\|Agenda Commands\\|Bookmark List\\)\\*\\'"
	   (display-buffer-at-bottom))))

(setopt flyspell-mode-map nil)

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'variable-pitch-mode)

(add-hook 'dired-mode-hook   #'hl-line-mode)
(add-hook 'ibuffer-mode-hook #'hl-line-mode)

(windmove-default-keybindings 'control)
(windmove-swap-states-default-keybindings '(shift control))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Global mirror mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tooltip-mode -1)

(tool-bar-mode -1)

(scroll-bar-mode -1)

(blink-cursor-mode -1)

(winner-mode +1)

(window-divider-mode +1)

(repeat-mode +1)

(save-place-mode +1)

(delete-selection-mode +1)

(auto-save-visited-mode +1)

(global-auto-revert-mode +1)

(pixel-scroll-precision-mode +1)

(add-hook 'after-init-hook   #'recentf-mode)

(add-hook 'after-init-hook   #'savehist-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Ui
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar yx/f-font "IBM Plex Mono")
(defvar yx/v-font "IBM Plex Sans")
(defvar yx/s-font "IBM Plex Serif")

(defun yx/setup-font ()
  (set-frame-font "Courier New-14" nil t)

  (set-face-attribute 'fixed-pitch nil :family yx/f-font)
  (set-face-attribute 'variable-pitch nil :family yx/v-font)
  (set-face-attribute 'fixed-pitch-serif nil :family yx/s-font)

  (cl-loop for font in '("LXGW WenKai" "Microsoft Yahei" "PingFang SC")
	   when (x-list-fonts font)
	   return (set-fontset-font t 'han (font-spec :family font)))

  (load-theme 'modus-operandi t))

(add-hook 'after-init-hook #'yx/setup-font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(keymap-global-unset "C-z")
(keymap-global-unset "M-'")

(keymap-global-set "C-/"	#'undo-only)
(keymap-global-set "M-/"	#'hippie-expand)

(keymap-global-set "C-x C-b"	#'ibuffer)

(keymap-global-set "C-z ."	#'repeat)
(keymap-global-set "C-z a"	#'org-agenda)
(keymap-global-set "C-z c"	#'org-capture)
(keymap-global-set "C-z l"	#'org-store-link)
(keymap-global-set "C-z s"	#'scratch-buffer)

(keymap-global-set "C-c d"	#'duplicate-dwim)

(keymap-global-set "M-' r"      #'raise-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/extras")

(require 'init-util)

(require 'init-base)

(require 'init-org+)

(require 'init-dev+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
