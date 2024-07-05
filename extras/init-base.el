;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-07-05 12:00:21
;; Modified: <2024-07-05 12:00:22 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package no-littering
  :ensure t
  :config (no-littering-theme-backups))

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
  :bind (("C-." . embark-act)))

(use-package consult
  :ensure t
  :bind (("C-x b"   . consult-buffer)
	 ("C-x 4 b" . consult-buffer-other-window)
	 ("C-x r b" . consult-bookmark)
	 ("M-r"     . consult-recent-file)
	 ("M-y"     . consult-yank-pop)
	 ("M-s r"   . consult-ripgrep)
	 ("M-s l"   . consult-line)
	 ("M-s M-l" . consult-line-multi)
	 ("M-g i"   . consult-imenu)
	 ("M-g g"   . consult-goto-line)
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


(provide 'init-base)
;;; init-base.el ends here
