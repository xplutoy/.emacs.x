;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-07-19 09:52:58
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package eat
  :init
  (setq eat-kill-buffer-on-exit t)
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(use-package gptel
  :bind (("C-c <return>" . gptel-send)
	 ("C-c C-<return>" . gptel-menu))
  :custom
  (gptel-use-curl nil)
  (gptel-default-mode 'org-mode)
  (gptel-backend (gptel-make-openai "kimi"
			:host "api.moonshot.cn"
			:key 'gptel-api-key
			:models '(moonshot-v1-32k)))
  (gptel-model 'moonshot-v1-32k)
  :config
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package minions
  :hook (after-init . minions-mode))

(use-package sis
  :config
  (add-to-list 'sis-prefix-override-keys "M-s")
  (add-to-list 'sis-prefix-override-keys "M-g")
  (when IS-MAC
    (sis-ism-lazyman-config
     "com.apple.keylayout.ABC"
     "com.apple.inputmethod.SCIM.Shuangpin"))
  (sis-global-inline-mode t)
  (sis-global-respect-mode t))

(use-package bing-dict
  :bind (("M-s d" . bing-dict-brief)))

(use-package olivetti
  :hook ((Info-mode . olivetti-mode)
	 (eww-mode . olivetti-mode)
	 (org-mode . olivetti-mode)
	 (org-agenda-mode . olivetti-mode))
  :custom
  (olivetti-style 'fancy)
  (olivetti-body-width 100)
  :config
  (setq olivetti-mode-map nil)
  (with-eval-after-load 'elfeed
    (add-hook 'elfeed-show-mode-hook #'olivetti-mode)))

(use-package elfeed
  :defer
  :custom
  (elfeed-feeds
   '(("https://planet.emacslife.com/atom.xml" emacs)
     ("http://xahlee.info/emacs/emacs/blog.xml" emacs)
     ("https://www.juliabloggers.com/feed/" julia)
     ("https://feeds.feedburner.com/RBloggers" R data-sci)
     ("https://python-bloggers.com/feed/" data-sci python)
     ("https://www.planetpython.org/rss20.xml" python)
     ("https://planet.lisp.org/rss20.xml" lisp)
     ("https://planet.scheme.org/atom.xml" scheme)
     ("https://planet.haskell.org/rss20.xml" haskell)
     ("https://www.cyberciti.biz/feed/" linux)
     ("https://eranraviv.com/feed/" math)
     ("https://writings.stephenwolfram.com/feed/" math)
     ("https://www.kdnuggets.com/feed" data-sci)
     ("https://towardsdatascience.com/feed" data-sci)
     ("https://machinelearningmastery.com/feed/" ai-new)
     ("https://www.marktechpost.com/feed/" ai-new)
     ("https://deepmind.google/blog/rss.xml" ai-new)
     ("https://bair.berkeley.edu/blog/feed.xml" ai-new)
     ("https://rss.arxiv.org/rss/cs.AI+cs.CV+cs.LG" ai-arxiv)
     ("https://wangyurui.com/feed.xml" life)
     ("https://www.ruanyifeng.com/blog/atom.xml" tech)))
  :config
  (add-hook 'elfeed-show-mode-hook (lambda () (setq line-spacing 0.2))))


(provide 'init-misc)
;;; init-misc.el ends here
