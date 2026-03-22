;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-07-19 09:52:58
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package no-littering
  :demand t
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory))
  (no-littering-theme-backups))

(use-package avy
  :bind (("M-g w" . avy-goto-word-1)
	 ("M-g l" . avy-goto-line)
	 ("M-g c" . avy-goto-char-timer)))

(use-package expreg
  :bind (("C-M-=" . expreg-expand)
	 ("C-M--" . expreg-contract)))

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
	 ([remap mark-sexp] . easy-mark)))

(use-package speedrect
  :hook (after-init . speedrect-mode))

(use-package eat
  :hook ((eshell-load . eat-eshell-mode)
	 (eshell-load . eat-eshell-visual-command-mode))
  :init (setopt eat-kill-buffer-on-exit t))

(use-package vundo
  :bind ("C-_" . vundo))

(use-package outli
  :vc (:url "https://github.com/jdtsmith/outli" :branch "main")
  :hook ((prog-mode text-mode) . outli-mode))

;;; UI

(use-package minions
  :hook (after-init . minions-mode))

;;; Misc

(use-package pyim
  :init
  (setq default-input-method "pyim")
  (pyim-default-scheme 'xiaohe-shuangpin)
  (setq pyim-english-input-switch-functions '(pyim-probe-org-speed-commands
					      pyim-probe-org-structure-template))
  (setq pyim-punctuation-half-width-functions '(pyim-probe-punctuation-line-beginning)))

(use-package bing-dict
  :bind ("M-s d" . bing-dict-brief)
  :init (setopt bing-dict-vocabulary-save t))

(use-package olivetti
  :hook ((Man-mode
	  Info-mode
	  org-mode
	  org-agenda-mode
	  eww-mode) . olivetti-mode)
  :init
  (setq olivetti-style 'nil)
  (setq olivetti-mode-map nil)
  (add-hook 'olivetti-mode-hook (lambda () (setq line-spacing 0.2))))


(provide 'init-tools)
;;; init-tools.el ends here
