;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 12:00:21
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package no-littering
  :ensure t
  :init (no-littering-theme-backups))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Edit
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :ensure t
  :bind (("M-g w" . avy-goto-word-1)
	 ("M-g l" . avy-goto-line)
	 ("M-g c" . avy-goto-char-timer)))

(use-package vundo
  :ensure t
  :bind (("C-c /" . vundo)))

(use-package expreg
  :ensure t
  :bind (("C-M-=" . expreg-expand)
         ("C-M--" . expreg-contract)))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
	 ([remap mark-sexp] . easy-mark)))

(use-package speedrect
  :ensure t
  :config (speedrect-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :ensure t
  :init
  (vertico-mode +1)
  :bind (:map vertico-map
	("RET" . vertico-directory-enter)
	("DEL" . vertico-directory-delete-char))
  :config
  (vertico-indexed-mode +1))

(use-package corfu
  :ensure t
  :hook (prog-mode . corfu-mode)
  :custom
  (corfu-auto t))

(use-package embark
  :ensure t
  :init
  (setq embark-cycle-key ".")
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)))

(use-package consult
  :ensure t
  :bind (("s-r"     . consult-recent-file)
         ("C-x b"   . consult-buffer)
	 ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
	 ("C-x p b" . consult-project-buffer)
	 ("C-x r b" . consult-bookmark)
         ("C-x t b" . consult-buffer-other-tab)
	 ("C-x M-:" . consult-complex-command)
	 ("M-X"     . consult-mode-command)
	 ("M-y"     . consult-yank-pop)
	 ("M-s f"   . consult-fd)
	 ("M-s r"   . consult-ripgrep)
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
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  (use-package embark-consult :ensure t))

(use-package consult-dir
  :after consult
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(basic orderless))
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))
                                        (file (styles basic partial-completion)))))


(provide 'init-base)
;;; init-base.el ends here
