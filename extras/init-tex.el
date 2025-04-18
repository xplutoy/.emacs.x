;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-04-18 19:33:16

;;; Commentary:

;;

;;; Code:

(use-package auctex
  :custom
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-engine 'xetex)
  (TeX-save-query nil)
  (reftex-plug-into-AUCTeX t))

(use-package cdlatex
  :init
  (with-eval-after-load 'org
    (keymap-set org-cdlatex-mode-map "$" #'cdlatex-dollar)
    (keymap-set org-cdlatex-mode-map "¥" #'cdlatex-dollar))
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex))


(provide 'init-tex)
;;; init-tex.el ends here
