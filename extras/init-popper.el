;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2026-08-29 18:27:37

;;; Commentary:

;;

;;; Code:

(use-package popper
  :bind (("C-`"   . popper-toggle)
	 ("M-`"   . popper-cycle)
	 ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
	'("Output\\*$"
	  "\\*Async Shell Command\\*"
	  "\\*eat\\*"
	  "^\\*eshell.*\\*$" eshell-mode
	  help-mode
	  compilation-mode))
  ;; (popper-group-function #'popper-group-by-project)
  :init
  (popper-mode +1)
  (popper-echo-mode +1))

(provide 'init-popper)
;;; init-popper.el ends here
