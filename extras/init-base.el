;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 12:00:21
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package no-littering
  :ensure t
  :config (no-littering-theme-backups))

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
  :bind (("M-' =" . expreg-expand)
         ("M-' -" . expreg-contract)))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
	 ([remap mark-sexp] . easy-mark)))

(use-package speedrect)
;; :vc (:url "https://github.com/jdtsmith/speedrect")

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
  :init
  (setq corfu-auto t)
  (global-corfu-mode +1))

(use-package embark
  :ensure t
  :init
  (setq embark-cycle-key ".")
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)))

(use-package consult
  :ensure t
  :bind (("C-x b"   . consult-buffer)
	 ("C-x 4 b" . consult-buffer-other-window)
	 ("C-x p b" . consult-project-buffer)
	 ("C-x r b" . consult-bookmark)
	 ("C-x M-:" . consult-complex-command)
	 ("M-X"     . consult-mode-command)
	 ("M-r"     . consult-recent-file)
	 ("M-y"     . consult-yank-pop)
	 ("M-s f"   . consult-fd)
	 ("M-s r"   . consult-ripgrep)
	 ("M-s l"   . consult-line)
	 ("M-s M-l" . consult-line-multi)
	 ("M-g i"   . consult-imenu)
	 ("M-g g"   . consult-goto-line)
	 ("M-g f"   . consult-flymake)
	 ("M-g h"   . consult-outline)
	 ("C-x '"   . consult-register-store)
	 ("M-g '"   . consult-register)
	 ("M-g M-'" . consult-register-load)
	 ("M-g m"   . consult-mark)
	 ("M-g M-m" . consult-global-mark))
  :config
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-bookmark
   consult-recent-file
   consult--source-buffer
   consult--source-recent-file :preview-key "M-.")
  (use-package embark-consult :ensure t))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(basic orderless))
  (setq completion-category-overrides '((file (styles basic partial-completion))
					(eglot (styles basic yx/orderless-with-initialism)))))

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map)
  :init
  (setq cape-dict-file (no-littering-expand-etc-file-name "google-10000-english.txt"))
  (add-hook 'completion-at-point-functions
            (cape-capf-super #'cape-abbrev #'cape-dabbrev #'cape-dict))
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-tex)
  (add-hook 'completion-at-point-functions #'cape-emoji))


(provide 'init-base)
;;; init-base.el ends here
