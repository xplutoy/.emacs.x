;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-07-19 09:52:58
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package eat
  :ensure t
  :init
  (setq eat-kill-buffer-on-exit t)
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(use-package gptel
  :ensure t
  :bind ("M-s g" . gptel-menu)
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
  :ensure t
  :hook (after-init . minions-mode))

(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds
   '(("https://planet.emacslife.com/atom.xml" emacs)
     ("https://feeds.feedburner.com/RBloggers" R)
     ("https://www.juliabloggers.com/feed/" julia)
     ("https://www.planetpython.org/rss20.xml" python)
     ("https://planet.lisp.org/rss20.xml" lisp)
     ("https://planet.scheme.org/atom.xml" scheme)
     ("https://planet.haskell.org/rss20.xml" haskell)
     ("https://www.cyberciti.biz/feed/" linux)
     ("https://eranraviv.com/feed/" math)
     ("https://writings.stephenwolfram.com/feed/" math)
     ("https://towardsdatascience.com/feed" data-sci)
     ("https://machinelearningmastery.com/feed/" ai-new)
     ("https://rss.arxiv.org/rss/cs.AI+cs.CV+cs.LG" ai-paper)))
  :hook (elfeed-show-mode . olivetti-mode))

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

(use-package sis
  :ensure t
  :config
  (add-to-list 'sis-prefix-override-keys "C-z")
  (add-to-list 'sis-prefix-override-keys "M-s")
  (add-to-list 'sis-prefix-override-keys "M-g")
  (when IS-MAC
    (sis-ism-lazyman-config
     "com.apple.keylayout.ABC"
     "com.apple.inputmethod.SCIM.Shuangpin"))
  (sis-global-inline-mode t)
  (sis-global-respect-mode t)
  (sis-global-context-mode t))

(use-package bing-dict
  :ensure t
  :bind (("M-s d" . bing-dict-brief)))

(provide 'init-misc)
;;; init-misc.el ends here
