;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-04-17 16:22:55

;;; Commentary:

;;

;;; Code:

(use-package denote
  :bind (("C-c n n"   . denote-subdirectory)
	 ("C-c n g"   . denote-grep)
	 ("C-c n o"   . denote-open-or-create)
	 ("C-c n i"   . denote-link-or-create)
	 ("C-c n l l" . denote-find-link)
	 ("C-c n l b" . denote-find-backlink)
	 ("C-c n l c" . denote-query-contents-link)
	 ("C-c n r"   . denote-rename-file-using-front-matter)
	 ("C-c n C-r" . denote-rename-file))
  :custom
  (denote-directory org-directory)
  (denote-known-keywords '("ai" "tool"))
  (denote-date-prompt-use-org-read-date t)
  (denote-org-store-link-to-heading nil)
  (denote-dired-directories-include-subdirectories t)
  :config
  (denote-rename-buffer-mode +1)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))

(use-package denote-org
  :bind (("C-c n l h" . denote-org-link-to-heading)
	 ("C-c n e s" . denote-org-extract-org-subtree)))

(use-package denote-journal
  :commands (denote-journal-calendar-mode)
  :init
  (setopt denote-journal-title-format nil)
  (keymap-global-set "C-c n j" #'denote-journal-new-entry)
  (add-hook 'calendar-mode-hook #'denote-journal-calendar-mode))

(use-package denote-sequence
  :bind (("C-c n s s" . denote-sequence)
	 ("C-c n s f" . denote-sequence-find)
	 ("C-c n s l" . denote-sequence-link)
	 ("C-c n s d" . denote-sequence-dired)
	 ("C-c n s r" . denote-sequence-reparent)
	 ("C-c n s c" . denote-sequence-convert))
  :config
  (setopt denote-sequence-scheme 'alphanumeric))

(provide 'init-denote)
;;; init-denote.el ends here
