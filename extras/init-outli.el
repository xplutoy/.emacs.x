;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2026-09-08 13:54:23

;;; Commentary:

;;

;;; Code:

(use-package outli
  :bind (:map outli-mode-map
	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :hook ((prog-mode text-mode) . outli-mode))

(provide 'init-outli)
;;; init-outli.el ends here
