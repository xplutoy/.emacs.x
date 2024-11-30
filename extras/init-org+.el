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

(setq org-directory "~/org-notes/")

(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

(setq org-cite-global-bibliography
      (list (expand-file-name "bibliography.bib" org-directory)))

(setq org-refile-targets
      '((nil :maxlevel . 2)
        (org-agenda-files :maxlevel . 2)))

(setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("@home" . ?h)
        ("@self" . ?s)
        ("@work" . ?w)
        (:endgroup . nil)))

(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)" "DONE(d!)")))

(setq org-capture-templates
      '(("h" "home"  entry (file+headline org-default-notes-file "Home") "* TODO %?" :prepend t)
        ("w" "work"  entry (file+headline org-default-notes-file "Work") "* TODO %?" :prepend t)))

(use-package org
  :init
  (setq org-crypt-key nil)
  (setq org-log-done 'time)
  (setq org-pretty-entities t)
  (setq org-log-into-drawer t)
  (setq org-hide-emphasis-markers t)
  (setq org-use-sub-superscripts '{})
  (setq org-support-shift-select t)
  (setq org-special-ctrl-k t)
  (setq org-special-ctrl-a/e t)
  (setq org-use-speed-commands t)
  (setq org-mouse-1-follows-link nil)
  (setq org-reverse-note-order t)
  (setq org-image-actual-width '(600))
  (setq org-footnote-auto-adjust t)
  (setq org-yank-adjusted-subtrees t)
  (setq org-M-RET-may-split-line nil)
  (setq org-todo-repeat-to-state "NEXT")
  (setq org-startup-with-latex-preview t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-insert-heading-respect-content t)
  (setq org-blank-before-new-entry '((heading 'auto)))
  (setq org-tags-exclude-from-inheritance '(crypt))
  (setq org-refile-use-outline-path 'file)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-attach-id-dir (expand-file-name "data" org-directory))
  (setq org-agenda-files '("inbox.org"))
  (setq org-agenda-diary-file (expand-file-name "diary.org" org-directory))
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-scheduled-leaders '("计划@-- " "拖延%03d "))
  (setq org-agenda-deadline-leaders  '("截止@-- " "剩余%03d " "逾期%03d "))
  (setq org-id-link-to-org-use-id 'create-if-interactive)
  (setq org-export-use-babel nil)
  (setq org-export-with-broken-links 'mark)
  (setq org-export-with-sub-superscripts '{})
  :config
  (require 'org-tempo)
  (org-crypt-use-before-save-magic)
  (add-hook 'org-mode-hook (lambda () (setq line-spacing 2)))
  (add-hook 'org-agenda-mode-hook #'hl-line-mode)
  (add-hook 'org-cycle-hook #'yx/org-toggle-inline-images-in-subtree)
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t) (emacs-lisp . t) (R . t)))
  (add-to-list 'org-babel-default-header-args '(:results . "silent") t)
  (add-to-list 'org-babel-default-header-args '(:eval . "never-export") t)
  :custom-face
  (org-level-1 ((t (:height 1.20))))
  (org-level-2 ((t (:height 1.15))))
  (org-level-3 ((t (:height 1.10))))
  (org-level-4 ((t (:height 1.05))))
  (org-document-title ((t (:height 1.30))))
  (org-done ((t (:strike-through t))))
  (org-headline-done ((t (:strike-through t)))))

(use-package ox-latex
  :init
  (setq org-latex-compiler "xelatex")
  (setq org-latex-packages-alist '(("" "amsfonts")))
  (setq org-cite-export-processors `((t csl ,(expand-file-name "ieee.csl" no-littering-etc-directory))))
  (setq org-preview-latex-image-directory (no-littering-expand-var-file-name "ltximg/"))
  :config
  (plist-put org-format-latex-options :scale 1.2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Org+
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package citeproc :ensure t)

(use-package org-ql
  :ensure t
  :bind (("C-c q v" . org-ql-view)
         ("C-c q s" . org-ql-search)
         ("C-c q q" . org-ql-find-in-org-directory)
         :map org-mode-map
         ("C-c q f" . org-ql-find)
         ("C-c q r" . org-ql-refile)
         ("C-c q l" . org-ql-open-link)))

(use-package valign
  :ensure t
  :hook (org-mode . valign-mode))

(use-package mixed-pitch
  :ensure t
  :hook (text-mode . mixed-pitch-mode))

(use-package org-modern
  :ensure t
  :custom
  (org-modern-table nil)
  (org-modern-block-fringe nil)
  :config
  (global-org-modern-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Writting and Reading
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package denote
  :ensure t
  :bind (("C-c n c"   . denote)
         ("C-c n t"   . denote-template)
         ("C-c n r"   . denote-rename-file)
         ("C-c n n"   . denote-open-or-create)
         ("C-c n i"   . denote-link-or-create)
         ("C-c n l"   . denote-find-link)
         ("C-c n C-l" . denote-find-backlink)
         ("C-c n M-l" . denote-backlinks)
         ("C-c n M-f" . denote-org-dblock-insert-links)
         ("C-c n M-b" . denote-org-dblock-insert-backlinks))
  :custom
  (denote-directory org-directory)
  (denote-known-keywords nil)
  (denote-date-prompt-use-org-read-date t)
  (denote-prompts '(subdirectory title keywords signature))
  :config
  (require 'denote-org-extras)
  (denote-rename-buffer-mode 1)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))

(use-package consult-denote
  :ensure t
  :bind (("C-c n f" . consult-denote-find)
         ("C-c n g" . consult-denote-grep))
  :init
  (consult-denote-mode +1)
  :config
  (when (executable-find "fd")
    (setq consult-denote-find-command #'consult-fd))
  (when (executable-find "rg")
    (setq consult-denote-grep-command #'consult-ripgrep)))

(use-package olivetti
  :ensure t
  :hook ((org-mode . olivetti-mode)
         (org-agenda-mode . olivetti-mode))
  :init
  (setq olivetti-style nil)
  (setq olivetti-mode-map nil))

(provide 'init-org+)
;;; init-org+.el ends here
