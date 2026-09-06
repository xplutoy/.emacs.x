;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2026-09-06 09:56:22

;;; Commentary:

;;

;;; Code:

;; (use-package eat
;;   :hook ((eat-mode . mode-line-invisible-mode)
;;	 (eshell-load . eat-eshell-mode)
;;	 (eshell-load . eat-eshell-visual-command-mode))
;;   :init (setopt eat-kill-buffer-on-exit t))

(use-package ghostel
  :bind
  ( :map ghostel-semi-char-mode-map
    ("M-o" . nil)
    ("M-s" . nil))
  :hook ((after-init . ghostel-comint-global-mode)
	 (eshell-load . ghostel-eshell-visual-command-mode)
	 (ghostel-mode . mode-line-invisible-mode))
  :config
  (setopt ghostel-shell (or (executable-find "pwsh") (getenv "SHELL")))
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t))

(provide 'init-terminal)
;;; init-terminal.el ends here
