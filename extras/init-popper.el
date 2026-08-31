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
   '("\\*Messages\\*$"
     "\\*Warnings\\*$"
     "Output\\*$"
     "\\*Backtrace\\*$"
     "\\*Apropos\\*$"
     "^\\*eldoc.*\\*$"
     "\\*[Wo]*Man.*\\*$"
     "\\*Async Shell Command\\*"
     "\\*Calendar\\*$"
     "^\\*Process List\\*$"
     "\\*eat\\*"
     "^\\*eshell.*\\*$" eshell-mode
     "^\\*.*shell.*\\*.*$" shell-mode
     "^\\*.*terminal.*\\*.*$" term-mode
     "\\*Agenda Commands\\*"
     "\\*Org Select\\*"
     "\\*Capture\\*" "^CAPTURE-.*\\.org*"
     "\\*Embark \\(Collect\\|Live\\):.*\\*$"
     "^\\*eldoc.*\\*$"
     help-mode
     grep-mode
     occur-mode
     bookmark-bmenu-mode
     flymake-diagnostics-buffer-mode
     inferior-python-mode
     comint-mode
     compilation-mode))
  ;; (popper-group-function #'popper-group-by-project)
  :init
  (popper-mode +1)
  (popper-echo-mode +1))

(provide 'init-popper)
;;; init-popper.el ends here
