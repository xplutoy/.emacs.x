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

(setopt word-wrap t)
(setopt word-wrap-by-category t)
(setopt require-final-newline t)
(setopt sentence-end-double-space nil)

(setopt use-short-answers t)
(setopt use-dialog-box nil)
(setopt inhibit-splash-screen t)
(setopt inhibit-startup-message t)
(setopt initial-scratch-message nil)

(setopt bidi-inhibit-bpa t)
(setopt bidi-paragraph-direction 'left-to-right)
(setopt inhibit-compacting-font-caches t)
(setopt read-process-output-max (* 1024 1024))
(setopt scroll-conservatively 101)
(setopt auto-revert-avoid-polling t)

(setopt tab-always-indent 'complete)
(setopt set-mark-command-repeat-pop t)
(setopt mouse-wheel-tilt-scroll t)
(setopt recentf-max-saved-items 100)
(setopt electric-pair-preserve-balance nil)
(setopt backward-delete-char-untabify-method 'hungry)
(setopt cua-rectangle-mark-key [C-S-return])

(setopt search-default-mode t)
(setopt isearch-lazy-count t)
(setopt isearch-yank-on-move 'shift)

(setopt auto-save-visited-interval 10)
(setopt global-auto-revert-non-file-buffers t)

(setopt use-package-always-defer t)
(setopt use-package-always-ensure t)
(setopt use-package-compute-statistics t)

(setopt epg-pinentry-mode 'loopback)
(setopt epa-file-encrypt-to user-mail-address)

(setopt switch-to-prev-buffer-skip 'this)
(setopt switch-to-buffer-obey-display-actions t)

(setopt enable-recursive-minibuffers t)
(setopt completion-auto-select 'second-tab)
(setopt read-extended-command-predicate #'command-completion-default-include-p)

(setopt user-mail-address "yangxue.cs@foxmail.com")
(setopt smtpmail-smtp-server "smtp.qq.com")
(setopt gnus-select-method '(nnimap "foxmail.cs" (nnimap-address "imap.qq.com")))

(setopt dired-dwim-target t)
(setopt dired-mouse-drag-files t)
(setopt dired-kill-when-opening-new-dired-buffer t)

(setopt reb-re-syntax 'string)
(setopt bookmark-save-flag t)
(setopt calendar-date-style 'iso)
(setopt dictionary-server "dict.org")
(setopt browse-url-browser-function #'eww-browse-url)

(setopt tab-bar-show 1)
(setopt tab-bar-close-button-show nil)
(setopt tab-line-close-button-show 'selected)

(add-to-list 'display-buffer-alist
	     '("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
	       (display-buffer-no-window)
	       (allow-no-window . t)))

(add-hook 'text-mode-hook #'abbrev-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook (lambda () (setq line-spacing 0.2)))

(add-hook 'eww-mode-hook #'variable-pitch-mode)
(add-hook 'eww-after-render-hook (lambda ()
				   (eww-readable)
				   (setq line-spacing 0.2)))

(windmove-default-keybindings 'control)
(windmove-swap-states-default-keybindings '(control shift))

(with-eval-after-load 'esh-module
  (add-to-list 'eshell-modules-list 'eshell-rebind))

(add-hook 'eshell-mode-hook (lambda ()
			      (eshell-hist-mode +1)
			      (keymap-local-set "C-l" #'eshell/clear)))

(add-hook 'after-save-hook #'whitespace-cleanup)

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

(dolist (charset '(han cjk-misc))
  (set-fontset-font t charset (font-spec :family "LXGW WenKai Mono")))

(setopt modus-themes-mixed-fonts t)
(setopt modus-themes-variable-pitch-ui t)
(setopt modus-themes-org-blocks 'tinted-background)

(setopt modus-themes-headings '((0 . (1.3))
				(1 . (1.2))
				(2 . (semibold 1.1))
				(t . (semibold 1.05))))

(load-theme 'modus-operandi t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(keymap-global-unset "C-z")

(keymap-global-set "C-/"      #'undo-only)
(keymap-global-set "M-/"      #'hippie-expand)
(keymap-global-set "M-o"      #'other-window)
(keymap-global-set "M-O"      #'other-frame)

(keymap-global-set "C-M-r"    #'raise-sexp)

(keymap-global-set "C-c a"    #'org-agenda)
(keymap-global-set "C-c c"    #'org-capture)
(keymap-global-set "C-c l"    #'org-store-link)

(keymap-global-set "C-c f"    #'ffap)
(keymap-global-set "C-c d"    #'duplicate-dwim)
(keymap-global-set "C-c s"    #'scratch-buffer)
(keymap-global-set "C-c r"    #'rename-visited-file)
(keymap-global-set "C-c z"    #'hs-toggle-hiding)
(keymap-global-set "C-c 3"    #'follow-delete-other-windows-and-split)

(keymap-global-set "C-x w f"  #'tear-off-window)

(keymap-global-set "M-s d"    #'dictionary-lookup-definition)
(keymap-global-set "M-s M-d"  #'dictionary-search)

(keymap-global-set "C-x C-b"  #'ibuffer)

(keymap-substitute global-map #'zap-to-char #'zap-up-to-char)
(keymap-substitute global-map #'upcase-word #'upcase-dwim)
(keymap-substitute global-map #'downcase-word #'downcase-dwim)
(keymap-substitute global-map #'capitalize-word #'capitalize-dwim)

(with-eval-after-load 'isearch
  (keymap-substitute isearch-mode-map #'isearch-delete-char #'isearch-del-char))

(defalias 'run-el #'ielm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   OS Specific
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when IS-WIN
  (setq w32-apps-modifier 'hyper)
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-t]))

(when IS-MAC
  (let ((paths
	 (with-temp-buffer
	   (call-process-shell-command "cat /etc/paths /etc/paths.d/*" nil t)
	   (split-string (buffer-string)))))
    (setenv "PATH" (mapconcat #'identity paths ":"))
    (setq exec-path (append paths (list exec-directory))))
  (menu-bar-mode -1))

(setenv "HTTP_PROXY"  "http://localhost:7890")
(setenv "HTTPS_PROXY" "http://localhost:7890")

(require 'server)
(or (server-running-p) (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core Extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(add-to-list 'load-path "~/.emacs.d/extras")

(require 'init-util)

(require 'init-base)

(require 'init-org+)

(require 'init-dev+)

(require 'init-misc)

(require 'init-blog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load custom-file t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
