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

(defvar my-org-attach-dir (expand-file-name "attachments" org-directory)
  "My global `org attach' directory.")

(setopt set-attach-id-dir my-org-attach-dir)
(setopt org-agenda-diary-file (file-name-concat org-directory "diary.org"))
(setopt org-preview-latex-image-directory (no-littering-expand-var-file-name "ltximg/"))
(setopt org-cite-global-bibliography `(,(expand-file-name "bibliography.bib" org-directory)))
(setopt org-yank-image-save-method (expand-file-name "images" my-org-attach-dir))

(setopt org-capture-templates
	'(("t" "Task" entry (file+headline "" "Tasks") "** TODO %?")
	  ("f" "Fleeting note" entry (file+headline "" "Notes") "** %?")))

(with-eval-after-load 'org

  (org-crypt-use-before-save-magic)

  (defun yx/org-mode-init-h ()
    "Initializes `org-mode' to my taste."
    (setq-local global-hl-line-mode nil)
    (setq-local electric-pair-pairs
		(append electric-pair-pairs '((?~ . ?~) (?+ . ?+))))
    (setq-local electric-pair-text-pairs electric-pair-pairs)
    ;; (variable-pitch-mode +1)
    (visual-line-mode +1)
    (visual-wrap-prefix-mode +1)
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
  (add-to-list 'org-structure-template-alist '("t"  . "theorem"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))

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

;;; Blog

(use-package htmlize)

(defvar my-blog-dir (expand-file-name "blog" org-directory))
(defvar my-blog-publish-dir "~/workspace/xplutoy.github.io/")

(defvar my-blog-head "<link rel='stylesheet' href='/static/org.css' type='text/css'/>")
(defvar my-blog-preamble '(("en" "<nav class='nav'> <a href='/about.html' class='button'>HOME</a> <a href='/index.html' class='button'>BLOG</a></nav><hr/>")))
(defvar my-blog-postamble "<div id='postamble'> <hr/> <p>Created with %c by YangXue <br\>Updated: %C<br/></p> </div>")

(defun yx/org-publish-sitemap-entry (entry style project)
  (cond ((not (directory-name-p entry))
	 (format "[%s] [[file:%s][%s]]"
		 (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
		 entry
		 (org-publish-find-title entry project)))
	((eq style 'tree)
	 (capitalize (file-name-nondirectory (directory-file-name entry))))
	(t entry)))

(setopt org-publish-project-alist
	`(("my-blog"
	   :components ("my-blog-posts" "my-blog-static"))
	  ("my-blog-posts"
	   :base-directory ,my-blog-dir
	   :publishing-directory ,my-blog-publish-dir
	   :publishing-function org-html-publish-to-html
	   :recursive t
	   :auto-sitemap t
	   :sitemap-filename "index.org"
	   :sitemap-title "Yx's Blog"
	   :sitemap-sort-files anti-chronologically
	   :sitemap-format-entry yx/org-publish-sitemap-entry
	   :html-head ,my-blog-head
	   :html-preamble-format ,my-blog-preamble
	   :html-postamble ,my-blog-postamble
	   :html-doctype "html5"
	   :html-html5-fancy t
	   :html-head-include-scripts nil
	   :html-head-include-default-style nil
	   :html-htmlize-output-type 'css
	   :with-smart-quotes t
	   :with-toc nil
	   :with-sub-superscript nil)
	  ("my-blog-static"
	   :base-directory ,my-blog-dir
	   :base-extension "css\\|js\\|png\\|jpg\\|gif"
	   :publishing-directory ,my-blog-publish-dir
	   :recursive t
	   :publishing-function org-publish-attachment)))

(defun yx/publish-blog (arg)
  "Publish my personal blog.
With the prefix argument ARG, forcing all posts to be republished."
  (interactive "P")
  (if arg
      (org-publish "my-blog" t nil)
    (org-publish "my-blog" nil nil))
  (message "Publish Done. Check output in %s." my-blog-publish-dir))


(provide 'init-writing)
;;; init-writing.el ends here
