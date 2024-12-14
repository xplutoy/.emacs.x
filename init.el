;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 11:18:22
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic Settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-WIN (eq system-type 'windows-nt))

(set-language-environment "UTF-8")

(setopt system-time-locale "C")
(setopt custom-file "~/.emacs.d/custom.el")

(setopt fill-column 78)
(setopt word-wrap t)
(setopt word-wrap-by-category t)
(setopt require-final-newline t)
(setopt indent-tabs-mode nil)
(setopt tab-always-indent 'complete)

(setopt use-short-answers t)
(setopt use-dialog-box nil)
(setopt inhibit-splash-screen t)
(setopt inhibit-startup-message t)
(setopt initial-scratch-message nil)
(setopt frame-title-format '("%b - GNU Emacs"))

(setopt bidi-inhibit-bpa t)
(setopt bidi-paragraph-direction 'left-to-right)
(setopt sentence-end-double-space nil)

(setopt reb-re-syntax 'string)
(setopt set-mark-command-repeat-pop t)
(setopt enable-recursive-minibuffers t)
(setopt find-file-visit-truename t)
(setopt delete-by-moving-to-trash t)
(setopt mouse-wheel-tilt-scroll t)
(setopt global-auto-revert-non-file-buffers t)

(setopt epg-pinentry-mode 'loopback)
(setopt epa-file-encrypt-to user-mail-address)

(setopt split-width-threshold 120)
(setopt switch-to-buffer-obey-display-actions t)
(setopt winner-boring-buffers-regexp "^\\*")

(setopt completion-auto-select 'second-tab)
(setopt read-extended-command-predicate #'command-completion-default-include-p)

(setopt user-mail-address "yangxue.cs@foxmail.com")
(setopt smtpmail-smtp-server "smtp.qq.com")
(setopt gnus-select-method '(nnimap "foxmail.cs" (nnimap-address "imap.qq.com")))

(setopt dired-dwim-target t)
(setopt dired-mouse-drag-files t)
(setopt dired-kill-when-opening-new-dired-buffer t)

(add-hook 'text-mode-hook #'abbrev-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'variable-pitch-mode)
(add-hook 'text-mode-hook (lambda () (hl-line-mode 'toggle)))

(windmove-default-keybindings 'control)
(windmove-swap-states-default-keybindings '(control shift))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Global Mirror Mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(repeat-mode +1)
(winner-mode +1)
(save-place-mode +1)
(auto-save-visited-mode +1)
(global-auto-revert-mode +1)

(cua-selection-mode +1)
(window-divider-mode +1)
(global-hl-line-mode +1)
(pixel-scroll-precision-mode +1)

(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Ui
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt nobreak-char-display nil)
(setopt mode-line-end-spaces '(:eval (if (display-graphic-p) " 　" "-%-")))

(set-face-attribute 'fixed-pitch nil :family "Iosevka")
(set-fontset-font t 'han (font-spec :family "LXGW WenKai Mono"))

(setopt modus-themes-mixed-fonts t)
(setopt modus-themes-variable-pitch-ui t)
(setopt modus-themes-org-blocks 'tinted-background)

(load-theme 'modus-operandi t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(keymap-global-unset "C-z")

(keymap-global-set "C-/"      #'undo-only)
(keymap-global-set "M-/"      #'hippie-expand)
(keymap-global-set "C-g"      #'yx/keyboard-quit)

(keymap-global-set "C-M-r"    #'raise-sexp)

(keymap-global-set "C-x C-b"  #'ibuffer)
(keymap-global-set "C-x C-d"  #'project-dired)

(keymap-global-set "C-c a"    #'org-agenda)
(keymap-global-set "C-c c"    #'org-capture)
(keymap-global-set "C-c l"    #'org-store-link)
(keymap-global-set "C-c d"    #'duplicate-dwim)
(keymap-global-set "C-c s"    #'scratch-buffer)
(keymap-global-set "C-c 3"    #'follow-delete-other-windows-and-split)

(with-eval-after-load 'flyspell
  (keymap-unset flyspell-mode-map "C-.")
  (keymap-unset flyspell-mode-map "C-,"))

(with-eval-after-load 'isearch
  (keymap-substitute isearch-mode-map #'isearch-delete-char #'isearch-del-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   OS Specific
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when IS-WIN
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-r]))

(when IS-MAC
  (let ((paths
         (with-temp-buffer
           (call-process-shell-command "cat /etc/paths /etc/paths.d/*" nil t)
           (split-string (buffer-string)))))
    (setenv "PATH" (mapconcat #'identity paths ":"))
    (setq exec-path (append paths (list exec-directory)))))

(setenv "HTTP_PROXY"  "http://localhost:7890")
(setenv "HTTPS_PROXY" "http://localhost:7890")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core Extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/extras")

(require 'init-util)

(require 'init-base)

(require 'init-org+)

(require 'init-dev+)

(require 'init-misc)

(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
