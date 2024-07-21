;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-07-19 09:52:58
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package gptel
  :ensure t
  :bind (("C-z z"   . gptel)
         ("C-z C-z" . gptel-menu))
  :config
  (setq gptel-use-curl nil)
  (setq gptel-default-mode 'org-mode)
  (setq gptel-backend (gptel-make-openai "KiMi"
                        :host "api.moonshot.cn"
                        :key 'gptel-api-key
                        :models '("moonshot-v1-32k" "moonshot-v1-128k")))
  (setq gptel-model "moonshot-v1-32k")
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package elfeed
  :ensure t
  :init
  (setq elfeed-feeds
   '(("https://spaces.ac.cn/feed" ai)
     ("https://lilianweng.github.io/index.xml" ai)
     ("https://tech.meituan.com/feed/" tech)
     ("https://ameow.xyz/feed/categories/weekly.xml" tech)
     ("https://www.ruanyifeng.com/blog/atom.xml" tech)
     ("https://planet.emacslife.com/atom.xml" emacs)))
  :hook (elfeed-show-mode . olivetti-mode))


(provide 'init-misc)
;;; init-misc.el ends here
