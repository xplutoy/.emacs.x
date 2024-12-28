;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 12:00:21
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory))
  (no-littering-theme-backups))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Edit
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :bind (("M-g w" . avy-goto-word-1)
	 ("M-g l" . avy-goto-line)
	 ("M-g c" . avy-goto-char-timer)))

(use-package vundo
  :bind (("C-c /" . vundo)))

(use-package expreg
  :bind (("C-M-=" . expreg-expand)
	 ("C-M--" . expreg-contract)))

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
	 ([remap mark-sexp] . easy-mark)))

(use-package speedrect
  :config (speedrect-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :init
  (vertico-mode +1)
  (vertico-indexed-mode +1)
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word))

(use-package corfu
  :custom
  (corfu-auto t)
  :hook ((prog-mode text-mode) . corfu-mode))

(use-package marginalia
  :init (marginalia-mode +1))

(use-package embark
  :bind (("C-." . embark-act)
	 ("M-." . embark-dwim))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package consult
  :bind (("s-r"     . consult-recent-file)
	 ("C-x b"   . consult-buffer)
	 ("C-x 4 b" . consult-buffer-other-window)
	 ("C-x 5 b" . consult-buffer-other-frame)
	 ("C-x p b" . consult-project-buffer)
	 ("C-x r b" . consult-bookmark)
	 ("C-x t b" . consult-buffer-other-tab)
	 ("C-x M-:" . consult-complex-command)
	 ("C-c h"   . consult-history)
	 ("M-X"     . consult-mode-command)
	 ("M-y"     . consult-yank-pop)
	 ("M-s f"   . consult-fd)
	 ("M-s r"   . consult-ripgrep)
	 ("M-s m"   . consult-man)
	 ("M-s i"   . consult-info)
	 ("M-s l"   . consult-line)
	 ("M-s k"   . consult-keep-lines)
	 ("M-s u"   . consult-focus-lines)
	 ("M-s M-l" . consult-line-multi)
	 ("M-g i"   . consult-imenu)
	 ("M-g g"   . consult-goto-line)
	 ("M-g f"   . consult-flymake)
	 ("M-g e"   . consult-compile-error)
	 ("M-g h"   . consult-outline)
	 ("M-g a"   . consult-org-agenda)
	 ("M-'"     . consult-register-store)
	 ("M-g '"   . consult-register)
	 ("M-g M-'" . consult-register-load)
	 ("M-g m"   . consult-mark)
	 ("M-g M-m" . consult-global-mark)
	 :map org-mode-map
	 ("M-g h"   . consult-org-heading))
  :config
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  (setq consult-preview-key nil)
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden")))

(use-package embark-consult :defer)

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
	 :map vertico-map
	 ("C-x C-d" . consult-dir)
	 ("C-x C-j" . consult-dir-jump-file)))

(use-package orderless
  :config
  (setq completion-styles '(basic orderless))
  (setq completion-category-overrides '((file (styles partial-completion))
					(eglot (styles orderless))
					(eglot-capf (styles orderless)))))


(provide 'init-base)
;;; init-base.el ends here
