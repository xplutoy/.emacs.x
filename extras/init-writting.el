;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-07-05 12:00:11
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package denote
  :ensure t
  :bind (("C-c n c"   . denote)
	 ("C-c n t"   . denote-template)
	 ("C-c n n"   . denote-open-or-create)
	 ("C-c n i"   . denote-link-or-create)
	 ("C-c n C-l" . denote-backlinks)
	 ("C-c n C-f" . denote-find-link)
	 ("C-c n C-b" . denote-find-backlink)
	 ("C-c n M-f" . denote-org-dblock-insert-links)
	 ("C-c n M-b" . denote-org-dblock-insert-backlinks))
  :custom
  (denote-directory yx/org-dir)
  (denote-known-keywords nil)
  (denote-date-prompt-use-org-read-date t)
  (denote-prompts '(subdirectory title keywords signature))
  :config
  (require 'denote-org-extras)
  (denote-rename-buffer-mode 1))

(use-package olivetti
  :ensure t
  :hook ((org-mode . olivetti-mode)
         (org-agenda-mode . olivetti-mode))
  :init
  (setq olivetti-style nil)
  (setq olivetti-mode-map nil))


(provide 'init-writting)
;;; init-writting.el ends here
