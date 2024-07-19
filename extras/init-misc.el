;;; -*- lexical-binding: t -*-

;; Author:  yangxue.cs@foxmail.com
;; Created: 2024-07-19 09:52:58
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package gptel
  :ensure t
  :bind (("C-z RET"   . gptel-menu))
  :config
  (setq gptel-use-curl nil)
  (setq gptel-default-mode 'org-mode)
  (setq gptel-org-branching-context t)
  (setq gptel-backend (gptel-make-openai "KiMi"
                        :host "api.moonshot.cn"
                        :key 'gptel-api-key
                        :models '("moonshot-v1-32k" "moonshot-v1-128k")))
  (setq gptel-model "moonshot-v1-32k"))


(provide 'init-misc)
;;; init-misc.el ends here
