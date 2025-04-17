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
	 ("C-c n l"   . denote-find-link)
	 ("C-c n C-l" . denote-find-backlink)
	 ("C-c n q c" . denote-query-contents-link)
	 ("C-c n r"   . denote-rename-file-using-front-matter)
	 ("C-c n C-r" . denote-rename-file))
  :custom
  (denote-directory org-directory)
  (denote-known-keywords '("tool" "robot" "ai"))
  (denote-org-store-link-to-heading nil)
  (denote-date-prompt-use-org-read-date t)
  (denote-dired-directories-include-subdirectories t)
  :config
  (denote-rename-buffer-mode +1)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))

(use-package denote-org)

(use-package denote-journal
  :init
  (setopt denote-journal-extras-title-format nil)
  (keymap-global-set "C-c n j" #'denote-journal-new-or-existing-entry)
  (add-hook 'calendar-mode-hook #'denote-journal-calendar-mode))

(use-package denote-sequence)

(provide 'init-denote)
;;; init-denote.el ends here
