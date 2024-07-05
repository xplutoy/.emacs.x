;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-07-05 12:00:03
;; Modified: <2024-07-05 12:00:04 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(setq org-directory yx/org-dir)

(setq org-agenda-files '("inbox.org" "work.org"))

(setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("@me" . ?m) ("@work" . ?t) ("@life" . ?l)
        (:endgroup . nil)
        ("crypt" . ?c) ("project" . ?p)))

(setq org-todo-keywords
      '((sequence "TODO(t!)" "SOMEDAY(s!)" "NEXT(n!)" "HOLD(h@/!)" "WAITING(w@/!)" "|" "CANCELED(c@/!)" "DONE(d!)")))

(setq org-capture-templates
      '(("d" "工作完成" entry (file+olp+datetree "work.org" "工作完成") "* %?")
	("w" "工作待办" entry (file+headline "work.org" "工作待办") "* TODO %?" :prepend t)
	("t" "个人事务" entry (file+headline org-default-notes-file "个人事务") "* TODO [#B] %?" :prepend t)
	("s" "未来想做" entry (file+headline org-default-notes-file "未来想做") "* SOMEDAY %?"   :prepend t)
	("h" "习惯养成" entry (file+headline org-default-notes-file "习惯养成") "* NEXT %?"      :prepend t)))

(use-package org
  :init
  (setq org-pretty-entities t)
  (setq org-startup-folded 'show2levels)
  (setq org-crypt-key nil)
  (setq org-special-ctrl-k t)
  (setq org-special-ctrl-a/e t)
  (setq org-yank-adjusted-subtrees t)
  (setq org-reverse-note-order t)
  (setq org-refile-targets '((nil :maxlevel . 2)))
  (setq org-refile-use-outline-path 'file)
  :config
  (require 'org-tempo)
  (org-crypt-use-before-save-magic)
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t) (emacs-lisp . t)))
  :custom-face
  (org-level-1 ((t (:height 1.3))))
  (org-level-2 ((t (:height 1.2))))
  (org-level-3 ((t (:height 1.1))))
  (org-document-title ((t (:height 1.5))))
  (org-done ((t (:strike-through t))))
  (org-headline-done ((t (:strike-through t)))))

(use-package ox-latex
  :init
  (setq org-latex-compiler "xelatex")
  (setq org-latex-default-class "ctexart")
  (setq org-latex-packages-alist '(("" "bm")
				   ("" "amsthm")
				   ("" "amsfonts")
				   ("" "xcolor" t)
				   ("cache=false" "minted" t)))
  (setq org-latex-pdf-process '("latexmk -f -xelatex -shell-escape -output-directory=%o %f"))
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-preview-latex-process-alist
	'((dvisvgm :programs
		   ("xelatex" "dvisvgm")
		   :description "xdv > svg"
		   :message "you need to install the programs: xelatex and dvisvgm."
		   :use-xcolor t
		   :image-input-type "xdv"
		   :image-output-type "svg"
		   :image-size-adjust (1.7 . 1.5)
		   :latex-compiler
		   ("xelatex -no-pdf -interaction nonstopmode -shell-escape -output-directory %o %f")
		   :image-converter
		   ("dvisvgm %f -e -n -b min -c %S -o %O"))))
  :config
  (add-to-list 'org-latex-classes
               '("ctexart"
                 "\\documentclass[11pt]{ctexart}"
                 ("\\section{%s}"       . "\\section*{%s}")
                 ("\\subsection{%s}"    . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))

(use-package valign
  :ensure t
  :hook (org-mode . valign-mode))

(use-package org-modern
  :ensure t
  :after org
  :config (global-org-modern-mode +1))


(provide 'init-org)
;;; init-org.el ends here
