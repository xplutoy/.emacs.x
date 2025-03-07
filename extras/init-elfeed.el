;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-07 21:21:36

;;; Commentary:

;;

;;; Code:

(use-package elfeed
  :hook (elfeed-show-mode . olivetti-mode)
  :custom
  (elfeed-curl-timeout 5)
  (elfeed-feeds '(("https://feeds.feedburner.com/RBloggers" R)
		  ("https://www.planetpython.org/rss20.xml" py)
		  ("https://www.juliabloggers.com/feed/" julia)
		  ("https://www.kdnuggets.com/feed" DS)
		  ("https://towardsdatascience.com/feed" DS)
		  ("https://www.jiqizhixin.com/rss" AI)
		  ("https://www.marktechpost.com/feed/" AI)
		  ("https://spaces.ac.cn/feed" math AI)
		  ("https://lilianweng.github.io/index.xml" AI)
		  ("https://planet.emacslife.com/atom.xml" emacs)
		  ("https://www.ruanyifeng.com/blog/atom.xml" IT)))
  :config

  (when IS-WIN
    (setopt elfeed-use-curl nil))

  (defun yx/elfeed-toggle-images ()
    "Toggle showing images."
    (interactive)
    (when (eq 'elfeed-show-mode major-mode)
      (setq shr-inhibit-images (not shr-inhibit-images))
      (elfeed-show-refresh)))

  (defun yx/elfeed-show-quit ()
    "Kill current elfeed-entry buffer and switch to elfeed-search buffer."
    (interactive)
    (when (derived-mode-p 'elfeed-show-mode)
      (kill-buffer (current-buffer))
      (switch-to-buffer (get-buffer "*elfeed-search*") nil t)))

  (defun yx/elfeed-show-external (&optional generic)
    "Visit the current entry in Xwidget or external browser with a prefix argument."
    (interactive "P")
    (when-let* ((link (elfeed-entry-link elfeed-show-entry)))
      (message "Sent to browser: %s" link)
      (if (and (not generic)
	       (featurep 'xwidget-internal))
	  (xwidget-webkit-browse-url link)
	(browse-url-default-browser link))))

  (keymap-set elfeed-show-mode-map "I" #'yx/elfeed-toggle-images)
  (keymap-set elfeed-show-mode-map "q" #'yx/elfeed-show-quit)
  (keymap-set elfeed-show-mode-map "&" #'yx/elfeed-show-external))


(provide 'init-elfeed)
;;; init-elfeed.el ends here
