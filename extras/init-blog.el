;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-12-15 06:39:56
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(defvar my-blog-dir (expand-file-name "blog" org-directory))

(defvar my-blog-publish-dir "~/workspace/xplutoy.github.io/")

(defvar my-blog-head "<link rel='stylesheet' href='./static/org.css' type='text/css'/> ")

(defvar my-blog-preamble '(("en" "<nav class='nav'> <a href='/about.html' class='button'>HOME</a> <a href='/index.html' class='button'>BLOG</a></nav><hr/>")))

(defvar my-blog-postamble "<div id='postamble'> <hr/> <p>Created with %c by YangXue <br\>Updated: %C<br/></p> </div>")

(setopt org-export-global-macros '(("timestamp" . "@@html:<span class=\"timestamp\">$1 </span>@@")))

(defun yx/org-sitemap-date-entry-format (entry style project)
  "Format ENTRY in org-publish PROJECT Sitemap format ENTRY ENTRY STYLE format that includes date."
  (let ((filename (org-publish-find-title entry project)))
    (if (= (length filename) 0)
	(format "*%s*" entry)
      (format "{{{timestamp(%s)}}} » [[file:%s][%s]]"
	      (format-time-string "%Y-%m-%d"
				  (org-publish-find-date entry project))
	      entry
	      filename))))

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
	   :sitemap-format-entry yx/org-sitemap-date-entry-format
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

(provide 'init-blog)
;;; init-blog.el ends here
