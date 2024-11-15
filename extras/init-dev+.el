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
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'prog-mode-hook #'show-paren-local-mode)
(add-hook 'prog-mode-hook #'toggle-truncate-lines)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'electric-pair-local-mode)
(add-hook 'prog-mode-hook #'electric-indent-local-mode)

(setq shell-kill-buffer-on-exit t)

(setq compilation-scroll-output 'first-error)
(setq compilation-auto-jump-to-first-error t)

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
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-stay-out-of 'yasnippet))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-dispatch)
         ("C-c v" . magit-file-dispatch))
  :init
  (setq magit-bury-buffer-function #'magit-restore-window-configuration)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package color-rg
  :load-path "elpa/color-rg"
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

(use-package buffer-env
  :unless IS-WIN
  :ensure t
  :init
  (add-hook 'comint-mode-hook #'buffer-env-update)
  (add-hook 'hack-local-variables-hook #'buffer-env-update)
  (setq buffer-env-script-name '(".envrc" ".venv/bin/activate")))

(use-package reformatter
  :ensure t)

;;;; python
(setq python-shell-dedicated 'project)
(setq python-indent-guess-indent-offset-verbose nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters "python")

(add-to-list 'treesit-language-source-alist
             '(python . ("https://github.com/tree-sitter/tree-sitter-python")))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(add-hook 'python-base-mode-hook #'eglot-ensure)

(reformatter-define black :program "black" :args '("-"))
(add-hook 'python-base-mode-hook #'black-on-save-mode)

;;;; julia
(use-package julia-mode
  :ensure t)

;;;; R
(use-package ess
  :ensure t
  :custom
  (ess-use-company nil)
  (ess-ask-for-ess-directory nil)
  (inferior-R-args "--quiet --no-save --no-restore")
  :config
  (require 'ess-julia)
  (require 'ess-r-mode)
  (keymap-set ess-r-mode-map ";" 'ess-insert-assign)
  (keymap-set inferior-ess-r-mode-map ";" 'ess-insert-assign)
  (add-hook 'ess-r-mode-hook #'eglot-ensure))

(provide 'init-dev+)
;;; init-dev+.el ends here
