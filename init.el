;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 11:18:22
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-WIN (eq system-type 'windows-nt))

(setopt user-full-name "xplutoyz")
(setopt user-mail-address "yangxue.cs@foxmail.com")

(setopt epa-file-encrypt-to user-mail-address)

(setopt smtpmail-smtp-server "smtp.qq.com")
(setopt gnus-select-method '(nnimap "foxmail.cs" (nnimap-address "imap.qq.com")))

(setopt display-buffer-alist '(("\\`\\*\\(Org Links\\|Compile-Log\\)\\*\\'"
				(display-buffer-no-window)
				(allow-no-window . t))))

(defun yx/global-minor-mode-init-h ()
  "Toggle frequently used global minor modes."
  (setopt abbrev-mode t)

  (which-key-mode +1)
  (global-completion-preview-mode +1)
  (global-prettify-symbols-mode +1)
  (global-so-long-mode +1)

  (repeat-mode +1)
  (winner-mode +1)
  (save-place-mode +1)
  (auto-save-visited-mode +1)
  (global-auto-revert-mode +1)

  (recentf-mode +1)
  (savehist-mode +1)
  (cua-selection-mode +1)
  (window-divider-mode +1)
  (global-hl-line-mode +1)
  (pixel-scroll-precision-mode +1))

(add-hook 'after-init-hook #'yx/global-minor-mode-init-h)

(windmove-default-keybindings 'control)
(windmove-swap-states-default-keybindings '(control shift))

;;; Keybindings

(keymap-global-unset "C-z")
(keymap-global-unset "<pinch>")
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")

(keymap-global-set "C-/"      #'undo-only)
(keymap-global-set "M-`"      #'other-frame)
(keymap-global-set "M-0"      #'delete-window)
(keymap-global-set "M-1"      #'delete-other-windows)
(keymap-global-set "M-["      #'scroll-other-window-down)
(keymap-global-set "M-]"      #'scroll-other-window)

(keymap-global-set "C-M-/"    #'hippie-expand)
(keymap-global-set "C-c a"    #'org-agenda)
(keymap-global-set "C-c c"    #'org-capture)
(keymap-global-set "C-c l"    #'org-store-link)
(keymap-global-set "C-c d"    #'duplicate-dwim)
(keymap-global-set "C-c s"    #'scratch-buffer)
(keymap-global-set "C-c h"    #'hs-toggle-hiding)

(keymap-global-set "C-x C-b"  #'ibuffer)

(let ((trans-keys '(("C-；" . "C-;")
		    ("C-。" . "C-.")
		    ("C-：" . "C-:")
		    ("C-，" . "C-,")
		    ("M-；" . "M-;")
		    ("M-。" . "M-.")
		    ("M-：" . "M-:")
		    ("M-，" . "M-,"))))
  (dolist (keys trans-keys)
    (keymap-set key-translation-map (car keys) (cdr keys))))

(with-eval-after-load 'isearch
  (keymap-substitute isearch-mode-map #'isearch-delete-char #'isearch-del-char))

;;; UI

(set-face-attribute 'fixed-pitch nil :family "Iosevka")

(dolist (charset '(han cjk-misc))
  (set-fontset-font t charset (font-spec :family "LXGW WenKai Mono GB Lite")))

(setopt modus-themes-mixed-fonts t)
(setopt modus-themes-variable-pitch-ui t)
(setopt modus-themes-org-blocks 'gray-background)

(load-theme 'modus-operandi t)

(setopt nobreak-char-display nil)
(setopt mode-line-end-spaces '(:eval (if (display-graphic-p) " 　" "-%-")))

;;; System

(require 'server)
(or (server-running-p) (server-start))

(setenv "HTTP_PROXY"  "http://localhost:7890")
(setenv "HTTPS_PROXY" "http://localhost:7890")

(defun yx/exec-path-and-PATH-update (paths)
  "Update `exec-path' and `PATH' environment variable."
  (setopt exec-path (append
		     (mapcar #'expand-file-name paths) exec-path))
  (setenv "PATH" (string-join exec-path path-separator)))

(when IS-MAC
  (let ((my-paths `("~/.local/bin"
		    "/Library/TeX/texbin"
		    ,(concat (invocation-directory) "bin"))))
    (yx/exec-path-and-PATH-update my-paths)))

;;; Build-in

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-avoid-polling t)
 '(auto-revert-verbose nil)
 '(auto-save-visited-interval 10)
 '(backward-delete-char-untabify-method 'hungry)
 '(bidi-paragraph-direction 'left-to-right)
 '(browse-url-browser-function 'eww-browse-url)
 '(calendar-date-style 'iso)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output 'first-error)
 '(completion-auto-select 'second-tab)
 '(completions-sort 'historical)
 '(cua-rectangle-mark-key [C-S-return])
 '(default-input-method "TeX")
 '(delete-by-moving-to-trash t)
 '(dired-dwim-target t)
 '(dired-kill-when-opening-new-dired-buffer t)
 '(dired-mouse-drag-files t)
 '(dired-movement-style 'cycle)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-autoshutdown t)
 '(eglot-extend-to-xref t)
 '(eglot-report-progress nil)
 '(enable-recursive-minibuffers t)
 '(epg-pinentry-mode 'loopback)
 '(fill-column 100)
 '(flymake-fringe-indicator-position 'right-fringe)
 '(frame-resize-pixelwise t)
 '(gdb-many-windows t)
 '(gdb-restore-window-configuration-after-quit t)
 '(global-auto-revert-non-file-buffers t)
 '(grep-use-headings t)
 '(gud-highlight-current-line t)
 '(imenu-flatten 'prefix)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(isearch-lazy-count t)
 '(isearch-yank-on-move 'shift)
 '(jit-lock-defer-time 0)
 '(minibuffer-visible-completions t)
 '(mouse-wheel-tilt-scroll t)
 '(org-M-RET-may-split-line nil)
 '(org-agenda-inhibit-startup t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-cite-export-processors '((t basic "numeric" "numeric")))
 '(org-crypt-key nil)
 '(org-export-dispatch-use-expert-ui t)
 '(org-export-use-babel nil)
 '(org-export-with-broken-links 'mark)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-footnote-auto-adjust t)
 '(org-goto-interface 'outline-path-completion)
 '(org-hide-emphasis-markers t)
 '(org-id-link-to-org-use-id 'create-if-interactive)
 '(org-image-actual-width nil)
 '(org-insert-heading-respect-content t)
 '(org-log-into-drawer t)
 '(org-modules '(ol-docview ol-eww ol-info))
 '(org-outline-path-complete-in-steps nil)
 '(org-pretty-entities t)
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-refile-targets '((nil :maxlevel . 2)))
 '(org-refile-use-outline-path 'file)
 '(org-reverse-note-order t)
 '(org-special-ctrl-a/e t)
 '(org-startup-folded 'show2levels)
 '(org-startup-with-latex-preview t)
 '(org-support-shift-select t)
 '(org-todo-keywords
   '((sequence "TODO(t!)" "NEXT(n!)" "WAIT(h@/!)" "|" "DONE(d!)")))
 '(org-use-speed-commands t)
 '(org-yank-adjusted-subtrees t)
 '(project-vc-extra-root-markers '(".dir-locals.el"))
 '(python-indent-guess-indent-offset-verbose nil)
 '(python-shell-dedicated 'project)
 '(read-extended-command-predicate 'command-completion-default-include-p)
 '(reb-re-syntax 'string)
 '(recentf-auto-cleanup 'never)
 '(recentf-max-saved-items 100)
 '(require-final-newline t)
 '(ring-bell-function 'ignore)
 '(scroll-conservatively 101)
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(shell-kill-buffer-on-exit t)
 '(show-paren-context-when-offscreen t)
 '(switch-to-buffer-obey-display-actions t)
 '(switch-to-prev-buffer-skip 'this)
 '(tab-always-indent 'complete)
 '(tab-bar-close-button-show nil)
 '(tab-bar-show 1)
 '(tab-line-close-button-show 'selected)
 '(truncate-lines t)
 '(use-dialog-box nil)
 '(use-package-always-defer t)
 '(use-package-always-ensure t)
 '(use-package-compute-statistics t)
 '(use-package-expand-minimally t)
 '(use-package-vc-prefer-newest t)
 '(use-short-answers t)
 '(vc-follow-symlinks t)
 '(vc-handled-backends '(Git))
 '(view-read-only t)
 '(winner-boring-buffers
   '("*Completions*" "*Help*" "*Buffer List*" "*Compile-Log*" "*Messages*" "*Backtrace*"))
 '(winner-boring-buffers-regexp "^magit")
 '(word-wrap-by-category t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:overline t :weight extrabold :height 1.2))))
 '(org-level-2 ((t (:weight bold :height 1.15))))
 '(org-level-3 ((t (:weight semibold :height 1.1))))
 '(org-level-4 ((t (:weight medium :height 1.05))))
 '(org-quote ((t (:inherit org-block :slant italic)))))

;;; Extras

(require 'package)
(require 'use-package-ensure)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'init-utils)

(require 'init-completion)

(require 'init-writing)

(require 'init-programing)

(require 'init-tools)

(load (locate-user-emacs-file "local-config.el") :noerror)
