;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-30 01:33:07

;;; Commentary:

;;

;;; Code:

(use-package ess
  :custom
  (ess-use-company nil)
  (ess-ask-for-ess-directory nil)
  (ess-history-directory no-littering-var-directory)
  (inferior-R-args "--quiet --no-save --no-restore")
  :config
  (require 'ess-julia)
  (keymap-set ess-r-mode-map ";" 'ess-insert-assign)
  (keymap-set inferior-ess-r-mode-map ";" 'ess-insert-assign)
  (add-hook 'ess-r-mode-hook #'eglot-ensure))

(provide 'init-ess)
;;; init-ess.el ends here
