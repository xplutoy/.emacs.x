;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 12:00:21
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package vertico
  :hook ((after-init . vertico-mode)
	 (minibuffer-setup . vertico-repeat-save))
  :bind (("M-R" . vertico-repeat)
	 :map vertico-map
	 ("M-N" . vertico-repeat-next)
	 ("M-P" . vertico-repeat-previous)
	 ("RET" . vertico-directory-enter)
	 ("DEL" . vertico-directory-delete-char))
  :config
  (vertico-indexed-mode +1)
  (vertico-multiform-mode +1))

(use-package corfu
  :hook ((prog-mode text-mode) . corfu-mode)
  :init (setopt corfu-auto t))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package embark
  :bind (("C-;" . embark-act)
	 ("C-." . embark-dwim)
	 ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package consult
  :bind (("C-x b"   . consult-buffer)
	 ("C-x f"   . consult-recent-file)
	 ("C-x 4 b" . consult-buffer-other-window)
	 ("C-x 5 b" . consult-buffer-other-frame)
	 ("C-x p b" . consult-project-buffer)
	 ("C-x r b" . consult-bookmark)
	 ("C-x t b" . consult-buffer-other-tab)
	 ("C-x M-:" . consult-complex-command)
	 ("M-X"     . consult-mode-command)
	 ("M-y"     . consult-yank-pop)
	 ("M-s r"   . consult-ripgrep)
	 ("M-s g"   . consult-git-grep)
	 ("M-s l"   . consult-line)
	 ("M-s M-l" . consult-line-multi)
	 ("M-s k"   . consult-keep-lines)
	 ("M-s u"   . consult-focus-lines)
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
	 ("M-g M-m" . consult-global-mark))
  :config
  (setq consult-preview-key nil)
  (consult-customize
   consult-line consult-line-multi :preview-key 'any)
  (with-eval-after-load 'org
    (keymap-set org-mode-map "M-g h" #'consult-org-heading))
    (setq xref-show-xrefs-function #'consult-xref)
  (setq	xref-show-definitions-function #'consult-xref))

(use-package embark-consult)

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
	 :map vertico-map
	 ("C-x C-d" . consult-dir)
	 ("C-x C-j" . consult-dir-jump-file)))

(use-package orderless
  :init
  (setq completion-styles '(basic orderless))
  (setq completion-category-overrides '((file (styles partial-completion))
					(eglot (styles orderless))
					(eglot-capf (styles orderless)))))


(provide 'init-completion)
;;; init-completion.el ends here
