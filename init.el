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

(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))

(set-language-environment "UTF-8")

(setopt user-mail-address "yangxue.cs@foxmail.com")

(setopt system-time-locale "C")
(setopt byte-compile-warnings nil)
(setopt gc-cons-threshold (* 32 1024 1024))
(setopt custom-file "~/.emacs.d/custom.el")

(setopt fill-column 78)
(setopt word-wrap t)
(setopt word-wrap-by-category t)
(setopt require-final-newline t)

(setopt frame-title-format '("%b - GNU Emacs"))
(setopt initial-scratch-message nil)
(setopt initial-major-mode 'fundamental-mode)

(setopt abbrev-mode t)
(setopt indent-tabs-mode nil)
(setopt tab-always-indent 'complete)

(setopt bidi-inhibit-bpa t)
(setopt bidi-paragraph-direction 'left-to-right)

(setopt reb-re-syntax 'string)
(setopt sentence-end-double-space nil)

(setopt save-silently t)
(setopt use-dialog-box nil)
(setopt use-short-answers t)
(setopt inhibit-splash-screen t)
(setopt inhibit-startup-message t)
(setopt eldoc-echo-area-use-multiline-p nil)

(setopt set-mark-command-repeat-pop t)
(setopt enable-recursive-minibuffers t)
(setopt find-file-visit-truename t)

(setopt recentf-auto-cleanup 300)
(setopt delete-by-moving-to-trash t)
(setopt global-auto-revert-non-file-buffers t)
(setopt mouse-drag-and-drop-region-cross-program t)

(setopt epg-pinentry-mode 'loopback)
(setopt epa-file-encrypt-to user-mail-address)

(setopt split-width-threshold 120)
(setopt switch-to-buffer-obey-display-actions t)
(setopt winner-boring-buffers-regexp "^\\*")

(setopt completion-auto-select 'second-tab)
(setopt read-extended-command-predicate #'command-completion-default-include-p)

(setopt display-buffer-alist
        '(("\\`\\*\\(Compile-Log\\|Org Links\\)\\*\\'"
           (display-buffer-no-window)
           (allow-no-window . t))
          ("\\`\\*\\(Org Select\\|Agenda Commands\\|Bookmark List\\)\\*\\'"
           (display-buffer-at-bottom))))

(setopt webjump-sites
        '(("ZhiHu"  . [simple-query "zhihu.com" "zhihu.com/search?q=" ""])
          ("GitHub" . [simple-query "github.com" "github.com/search?q=" ""])
          ("DuckGo" . [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])))

(setopt show-paren-style 'parenthesis)

(setopt dired-dwim-target t)
(setopt dired-mouse-drag-files t)
(setopt dired-kill-when-opening-new-dired-buffer t)

(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'dired-mode-hook #'dired-omit-mode)

(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'variable-pitch-mode)

(add-hook 'ibuffer-mode-hook #'hl-line-mode)

(windmove-default-keybindings 'control)
(windmove-swap-states-default-keybindings '(control shift))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Global mirror mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(repeat-mode +1)
(winner-mode +1)

(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode +1)
(window-divider-mode +1)

(save-place-mode +1)
(auto-save-visited-mode +1)
(global-auto-revert-mode +1)

(delete-selection-mode +1)
(pixel-scroll-precision-mode +1)

(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/extras")

(require 'init-util)

(when IS-MAC
  (yx/set-exec-path-from-shell))

(add-hook 'emacs-startup-hook #'yx/proxy-http-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Ui
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt nobreak-char-display nil)
(setopt mode-line-end-spaces '(:eval (if (display-graphic-p) " 　" "-%-")))

(set-face-attribute 'default nil :family "Intel One Mono" :height 150)
(set-face-attribute 'variable-pitch nil :family "IBM Plex Serif")

(setopt face-font-rescale-alist '(("LXGW WenKai Mono"  . 1.2)))
(set-fontset-font t 'han (font-spec :family "LXGW WenKai Mono"))

(setopt modus-themes-org-blocks 'tinted-background)

(load-theme 'modus-operandi t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(keymap-global-unset "C-z")
(keymap-global-unset "C-t")

(keymap-global-set "C-/"	#'undo-only)
(keymap-global-set "M-/"	#'hippie-expand)
(keymap-global-set "C-g"        #'yx/keyboard-quit)

(keymap-global-set "C-M-r"      #'raise-sexp)

(keymap-global-set "C-x C-b"	#'ibuffer)
(keymap-global-set "C-x C-d"    #'project-dired)

(keymap-global-set "C-c d"	#'duplicate-dwim)
(keymap-global-set "C-c 3"      #'follow-delete-other-windows-and-split)

(keymap-global-set "C-z ."	#'repeat)
(keymap-global-set "C-z /"	#'webjump)
(keymap-global-set "C-z a"	#'org-agenda)
(keymap-global-set "C-z c"	#'org-capture)
(keymap-global-set "C-z l"	#'org-store-link)
(keymap-global-set "C-z s"	#'scratch-buffer)

(keymap-global-set "C-t f"      #'follow-mode)
(keymap-global-set "C-t h"      #'highlight-changes-mode)
(keymap-global-set "C-t l"      #'flymake-mode)

(with-eval-after-load 'flyspell
  (keymap-unset flyspell-mode-map "C-.")
  (keymap-unset flyspell-mode-map "C-,"))

(with-eval-after-load 'isearch
  (keymap-substitute isearch-mode-map #'isearch-delete-char #'isearch-del-char))

(when IS-WIN
      (setq w32-lwindow-modifier 'super)
      (w32-register-hot-key [s-r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-base)

(require 'init-org+)

(require 'init-dev+)

(require 'init-misc)

(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
