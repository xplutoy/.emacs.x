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

(setopt org-agenda-files '("inbox.org"))

(setopt org-cite-global-bibliography
        (list (expand-file-name "bibliography.bib" org-directory)))

(setopt org-refile-targets
        '((nil :maxlevel . 2)
          (org-agenda-files :maxlevel . 2)))

(setopt org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)" "DONE(d!)")))

(setopt org-capture-templates
        '(("h" "home" entry (file+headline "inbox.org" "Home") "* TODO %?" :prepend t)
          ("w" "work" entry (file+headline "inbox.org" "Work") "* TODO %?" :prepend t)))

(use-package org
  :custom
  (org-log-into-drawer t)
  (org-hide-emphasis-markers t)
  (org-special-ctrl-a/e t)
  (org-use-speed-commands t)
  (org-support-shift-select t)
  (org-mouse-1-follows-link nil)
  (org-M-RET-may-split-line nil)
  (org-yank-adjusted-subtrees t)
  (org-image-actual-width nil)
  (org-footnote-auto-adjust t)
  (org-startup-align-all-tables t)
  (org-startup-with-latex-preview t)
  (org-refile-use-outline-path 'file)
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)
  (org-insert-heading-respect-content t)
  (org-tags-exclude-from-inheritance '(crypt))
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-export-use-babel nil)
  (org-export-with-broken-links 'mark)
  :config
  (setopt org-crypt-key nil)
  (org-crypt-use-before-save-magic)
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
  (org-document-title ((t (:height 1.30)))))

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
         ("C-c n r"   . denote-rename-file)
         ("C-c n n"   . denote-open-or-create)
         ("C-c n i"   . denote-link-or-create)
         ("C-c n l"   . denote-find-link)
         ("C-c n C-l" . denote-find-backlink)
         ("C-c n C-f" . denote-org-dblock-insert-links)
         ("C-c n C-b" . denote-org-dblock-insert-backlinks))
  :custom
  (denote-directory org-directory)
  (denote-known-keywords nil)
  (denote-date-prompt-use-org-read-date t)
  (denote-prompts '(subdirectory title keywords signature))
  :config
  (require 'denote-org-extras)
  (denote-rename-buffer-mode 1)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))


(provide 'init-org+)
;;; init-org+.el ends here
