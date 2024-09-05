;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 11:59:57
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(setq vc-follow-symlinks t)
(setq vc-handled-backends '(Git))

(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'show-paren-local-mode)
(add-hook 'prog-mode-hook #'electric-pair-local-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setq major-mode-remap-alist '((python-mode . python-ts-mode)))

(setq ediff-window-setup-function #'ediff-setup-windows-plain)
(setq ediff-split-window-function #'split-window-horizontally)

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(setq auto-insert-alist nil)

(define-skeleton yx/py-header  ""
  nil
  "# --------------------------------------------------"
  "\n# Author:      " (user-mail-address)
  "\n# Date:        " (format-time-string "%F %T")
  "\n# Description: " (skeleton-read "Description: ")
  "\n#\n#\n"
  "# --------------------------------------------------"
  "\n\n" @ _ "\n")

(define-skeleton yx/el-header  ""
  nil
  ";;; -*- lexical-binding: t -*-\n"
  "\n;; Author:  " (message-user-mail-address)
  "\n;; Created: " (format-time-string "%F %T")
  "\n;; License: GPLv3"
  "\n\n;;; Commentary:\n\n;; " @ _
  "\n\n;;; Code:"
  "\n\n(provide '" (file-name-base (buffer-file-name)) ")"
  "\n;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")

(define-auto-insert "\\.el$" #'yx/el-header)
(define-auto-insert "\\.py$" #'yx/py-header)


(use-package eglot
  :custom
  (eglot-report-progress nil)
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config
  (fset #'jsonrpc--log-event #'ignore))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-file-dispatch))
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
