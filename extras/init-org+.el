;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 12:00:03
;; License: GPLv3

;;; Commentary:

;;

;;; Code:


;;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt org-directory "~/org-notes/")
(setopt org-default-notes-file (file-name-concat org-directory "inbox.org"))
(setopt org-agenda-files `(,org-default-notes-file))

(setopt set-attach-di-dir (concat org-directory "data/"))
(setopt org-preview-latex-image-directory (no-littering-expand-var-file-name "ltximg/"))
(setopt org-cite-global-bibliography `(,(expand-file-name "bibliography.bib" org-directory)))

(with-eval-after-load 'org

  (org-crypt-use-before-save-magic)

  (defun yx/org-mode-init-h ()
    (visual-line-mode +1)
    (variable-pitch-mode +1)
    (visual-wrap-prefix-mode +1)
    (setq-local global-hl-line-mode nil)
    (setq-local electric-pair-pairs
		(append electric-pair-pairs '((?~ . ?~) (?+ . ?+))))
    (setq-local electric-pair-text-pairs electric-pair-pairs)
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))

  (add-hook 'org-mode-hook #'yx/org-mode-init-h)

  (with-eval-after-load 'completion-preview
    (push 'org-self-insert-command completion-preview-commands))

  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
			       (emacs-lisp . t)
			       (R . t)
			       (julia . t)))
  (add-to-list 'org-babel-default-header-args '(:eval . "no-export") t)

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("R"  . "src R"))
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))

  (plist-put org-format-latex-options :scale 1.5)

  (require 'ox-latex)
  (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}"
				    ("\\section{%s}" . "\\section*{%s}")
				    ("\\subsection{%s}" . "\\subsection*{%s}")
				    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				    ("\\paragraph{%s}" . "\\paragraph*{%s}")
				    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (defun yx/yank-image-file-name-f ()
    (concat (format-time-string "%Y%m%dT%H%M%S") "_"
	    (read-string "Caption: " nil nil "screenshot")))

  (setopt org-yank-image-file-name-function #'yx/yank-image-file-name-f)
  (setopt org-yank-image-save-method (concat org-directory "data/images/"))

  (defun yx/org-toggle-inline-images-in-subtree (state &optional beg end)
    "Refresh inline image previews in the current heading/tree."
    (let* ((beg (or beg
		    (if (org-before-first-heading-p)
			(save-excursion (point-min))
		      (save-excursion (org-back-to-heading) (point)))))
	   (end (or end
		    (if (org-before-first-heading-p)
			(save-excursion (org-next-visible-heading 1) (point))
		      (save-excursion (org-end-of-subtree) (point)))))
	   (overlays (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-image-overlay))
				       (ignore-errors (overlays-in beg end)))))
      (if (and (eq state 'subtree) (not overlays))
	  (org-display-inline-images t t beg end)
	(dolist (ov overlays nil)
	  (delete-overlay ov)
	  (setq org-inline-image-overlays (delete ov org-inline-image-overlays))))))

  (add-hook 'org-cycle-hook #'yx/org-toggle-inline-images-in-subtree))

;;; Org+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package htmlize)

(use-package auctex
  :custom
  (TeX-engine 'xetex)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (reftex-plug-into-AUCTeX t))

(use-package cdlatex
  :init
  (with-eval-after-load 'org
    (keymap-set org-cdlatex-mode-map "$" #'cdlatex-dollar)
    (keymap-set org-cdlatex-mode-map "¥" #'cdlatex-dollar))
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex))

(use-package denote
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
	 ("C-c n C-b" . denote-org-dblock-insert-backlinks))
  :custom
  (denote-directory org-directory)
  (denote-org-store-link-to-heading nil)
  (denote-date-prompt-use-org-read-date t)
  (denote-journal-extras-title-format nil)
  (denote-dired-directories-include-subdirectories t)
  :config
  (with-eval-after-load 'dired
    (keymap-set dired-mode-map "C-c n r" #'denote-dired-rename-files)
    (keymap-set dired-mode-map "C-c n k" #'denote-dired-rename-marked-files-with-keywords)
    (keymap-set dired-mode-map "C-c n C-r" #'denote-dired-rename-marked-files))
  (denote-rename-buffer-mode 1)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))


(provide 'init-org+)
;;; init-org+.el ends here
