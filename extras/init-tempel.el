;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-30 01:34:55

;;; Commentary:

;;

;;; Code:

(use-package tempel
  :bind (("M-*" . tempel-insert)
	 ("M-+" . tempel-complete))
  :init
  (setopt tempel-trigger-prefix "<")
  (defun yx/tempel-setup-capf ()
    (setq-local completion-at-point-functions
		(cons #'tempel-expand
		      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'yx/tempel-setup-capf)
  (add-hook 'text-mode-hook 'yx/tempel-setup-capf))

(provide 'init-tempel)
;;; init-tempel.el ends here
