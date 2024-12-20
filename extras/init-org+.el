;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 12:00:03
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Org
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt org-directory "~/org-notes/")
(setopt org-default-notes-file (file-name-concat org-directory "inbox.org"))
(setopt org-agenda-files `(,org-default-notes-file))

(setopt org-todo-keywords
	'((sequence "TODO(t!)" "NEXT(n!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)" "DONE(d!)")))

(setopt org-refile-targets
	'((nil :maxlevel . 2)
	  (org-agenda-files :maxlevel . 2)))

(setopt org-capture-templates
	'(("h" "home" entry (file+headline "Home") "* TODO %?" :prepend t)
	  ("w" "work" entry (file+headline "Work") "* TODO %?" :prepend t)))

(setopt org-log-into-drawer t)

(setopt org-image-actual-width nil)

(setopt org-hide-emphasis-markers t)

(setopt org-startup-align-all-tables t)
(setopt org-startup-with-latex-preview t)

(setopt org-special-ctrl-a/e t)
(setopt org-use-speed-commands t)
(setopt org-support-shift-select t)
(setopt org-M-RET-may-split-line nil)

(setopt org-yank-adjusted-subtrees t)
(setopt org-footnote-auto-adjust t)
(setopt org-insert-heading-respect-content t)

(setopt org-refile-use-outline-path 'file)
(setopt org-goto-interface 'outline-path-completion)
(setopt org-outline-path-complete-in-steps nil)

(setopt org-use-tag-inheritance nil)
(setopt org-agenda-skip-scheduled-if-deadline-is-shown t)

(setopt org-latex-compiler "xelatex")
(setopt org-latex-packages-alist '(("" "amsfonts")))
(setopt org-preview-latex-image-directory (no-littering-expand-var-file-name "ltximg"))

(setopt org-cite-global-bibliography `(,(expand-file-name "bibliography.bib" org-directory)))
(setopt org-cite-export-processors `((t csl ,(no-littering-expand-etc-file-name "ieee.csl"))))

(setopt org-export-use-babel nil)
(setopt org-export-with-broken-links 'mark)

(with-eval-after-load 'org

  (setopt org-crypt-key nil)
  (org-crypt-use-before-save-magic)

  (plist-put org-format-latex-options :scale 1.3)

  (add-hook 'org-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'org-cycle-hook #'yx/org-toggle-inline-images-in-subtree)

  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
			       (emacs-lisp . t)
			       (R . t)
			       (julia . t)))
  (add-to-list 'org-babel-default-header-args '(:eval . "no-export") t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Org+
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package citeproc :ensure t)

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . eglot-ensure)
	 (LaTeX-mode . turn-on-cdlatex)
	 (LaTeX-mode . prettify-symbols-mode))
  :custom
  (Tex-master 'dwim)
  (TeX-engine 'xetex)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (reftex-plug-into-AUCTeX t))

(use-package cdlatex
  :ensure t
  :hook ((LaTeX-mode . turn-on-cdlatex)
	 (org-mode . turn-on-org-cdlatex)))

(use-package denote
  :ensure t
  :bind (("C-c n c"   . denote)
	 ("C-c n n"   . denote-subdirectory)
	 ("C-c n j"   . denote-journal-extras-new-entry)
	 ("C-c n o"   . denote-open-or-create)
	 ("C-c n i"   . denote-link-or-create)
	 ("C-c n l"   . denote-find-link)
	 ("C-c n C-l" . denote-find-backlink)
	 ("C-c n r"   . denote-rename-file-using-front-matter)
	 ("C-c n C-r" . denote-rename-file)
	 ("C-c n C-f" . denote-org-dblock-insert-links)
	 ("C-c n C-b" . denote-org-dblock-insert-backlinks)
	 :map dired-mode-map
	 ("C-c n C-r" . denote-dired-rename-files)
	 ("C-c n r"   . denote-dired-rename-marked-files-using-front-matter)
	 ("C-c n k"   . denote-dired-rename-marked-files-with-keywords))
  :custom
  (denote-directory org-directory)
  (denote-date-prompt-use-org-read-date t)
  (denote-journal-extras-title-format nil)
  :config
  (denote-rename-buffer-mode 1)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))


(provide 'init-org+)
;;; init-org+.el ends here
