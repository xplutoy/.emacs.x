;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-07-19 09:52:58
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package eat
  :hook ((eshell-mode . eat-eshell-mode)
	 (eshell-mode . eat-eshell-visual-command-mode))
  :init (setopt eat-kill-buffer-on-exit t))

(use-package vundo
  :bind ("C-_" . vundo))

(use-package outli
  :vc (:url "https://github.com/jdtsmith/outli")
  :hook ((prog-mode
	  text-mode) . outli-mode))

;;; UI

(use-package minions
  :hook (after-init . minions-mode))

(use-package ef-themes
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t))

;;; AI

(use-package gptel
  :bind (("C-c <return>" . gptel-send)
	 ("C-c C-<return>" . gptel-menu))
  :custom
  (gptel-model 'deepseek-chat)
  (gptel-backend (gptel-make-openai "DeepSeek"
		   :host "api.deepseek.com"
		   :key 'gptel-api-key
		   :endpoint "/chat/completions"
		   :models '(deepseek-chat deepseek-coder)))
  (gptel-default-mode 'org-mode)
  :config
  (when IS-WIN
    (setopt gptel-use-curl nil))
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(defun yx/gptel-quick (capture-text &optional input)
  "Explain or summarize region or thing at point with an LLM."
  (interactive
   (list (cond
	  ((use-region-p) (buffer-substring-no-properties (region-beginning)
							  (region-end)))
	  ((derived-mode-p 'prog-mode) (thing-at-point 'defun t))
	  (t (thing-at-point 'sentence t)))
	 current-prefix-arg))
  (require 'gptel)
  (let* ((default-prompt  "请用精炼的中文解释和总结下面的内容：\n")
	 (llm-name (gptel-backend-name gptel-backend))
	 (query-text (if input
			 (read-string "Ask %s: llm-name")
		       (concat default-prompt capture-text)))
	 (gptel-use-context nil)
	 (gptel-max-tokens (+ (floor (length query-text) 8) 256)))
    (message "Query %s ..."  llm-name)
    (gptel-request query-text
		   :callback (lambda (resp info)
			       (message "Query %s Done!" llm-name)
			       (if (length< resp 20)
				   (message "%s: %s" llm-name resp)
				 (with-current-buffer
				     (get-buffer-create "*gptel-quick*")
				   (erase-buffer)
				   (insert resp)
				   (display-buffer (current-buffer))))))))

(keymap-global-set "M-s q" #'yx/gptel-quick)

;;; Chinese

(use-package sis
  :demand t
  :config
  (add-to-list 'sis-prefix-override-keys "M-s")
  (add-to-list 'sis-prefix-override-keys "M-g")
  (when IS-MAC
    (sis-ism-lazyman-config
     "com.apple.keylayout.ABC"
     "com.apple.inputmethod.SCIM.Shuangpin"))
  (sis-global-inline-mode +1)
  (sis-global-respect-mode +1))

(use-package bing-dict
  :bind (("M-s d" . bing-dict-brief)))

;;; Reading

(use-package olivetti
  :hook ((Man-mode
	  Info-mode
	  org-mode
	  org-agenda-mode
	  eww-mode) . olivetti-mode)
  :init
  (setq olivetti-style 'fancy)
  (setq olivetti-mode-map nil)
  (add-hook 'olivetti-mode-hook
	    (lambda () (setq line-spacing 0.2))))

(use-package elfeed
  :hook (elfeed-show-mode . olivetti-mode)
  :custom
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

  (keymap-set elfeed-show-mode-map "q" #'yx/elfeed-show-quit)
  (keymap-set elfeed-show-mode-map "&" #'yx/elfeed-show-external))


(provide 'init-misc)
;;; init-misc.el ends here
