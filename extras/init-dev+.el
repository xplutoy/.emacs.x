;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-07-05 11:59:57
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(setq vc-follow-symlinks t)
(setq vc-handled-backends '(Git))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setq major-mode-remap-alist '((python-mode . python-ts-mode)))

(add-hook 'prog-mode-hook #'electric-pair-local-mode)

(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config
  (fset #'jsonrpc--log-event #'ignore))

(use-package magit
  :ensure t
  :bind (("C-c v" . magit-file-dispatch))
  :init
  (setq magit-bury-buffer-function #'magit-restore-window-configuration)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package color-rg
  ;; :vc (:url "https://github.com/manateelazycat/color-rg")
  :bind(("M-s s"   . color-rg-search-input)
        ("M-s p"   . color-rg-search-input-in-project))
  :custom
  (color-rg-recenter-match-line t)
  (color-rg-search-no-ignore-file nil)
  (color-rg-mac-load-path-from-shell nil)
  :config
  (when IS-WIN
    (setq color-rg-command-prefix "powershell")))


(provide 'init-dev+)
;;; init-dev+.el ends here
