;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 11:59:57
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(show-paren-mode +1)
(electric-pair-mode +1)
(electric-indent-mode +1)

(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'toggle-truncate-lines)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setopt vc-follow-symlinks t)
(setopt vc-handled-backends '(Git))

(setopt gdb-many-windows t)
(setopt gdb-restore-window-configuration-after-quit t)
(setopt gud-highlight-current-line t)

(setopt shell-kill-buffer-on-exit t)

(setopt compilation-scroll-output 'first-error)
(setopt compilation-auto-jump-to-first-error t)

(setopt ediff-window-setup-function #'ediff-setup-windows-plain)
(setopt ediff-split-window-function #'split-window-horizontally)

(setopt auto-insert-alist nil)

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

(with-eval-after-load 'eglot
  (setopt eglot-autoshutdown t)
  (setopt eglot-extend-to-xref t)
  (setopt eglot-report-progress nil)
  (setopt eglot-send-changes-idle-time 0.1)
  (fset #'jsonrpc--log-event #'ignore)
  (keymap-set eglot-mode-map "C-c l r" #'eglot-rename)
  (keymap-set eglot-mode-map "C-c l f" #'eglot-format)
  (keymap-set eglot-mode-map "C-c l a" #'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c l h" #'eglot-help-at-point))

(with-eval-after-load 'flymake
  (setopt flymake-fringe-indicator-position 'right-fringe)
  (keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)
  (keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "C-c l d" #'flymake-show-buffer-diagnostics))

(use-package magit
  :bind (("C-c g" . magit-dispatch)
         ("C-c v" . magit-file-dispatch))
  :custom
  (magit-clone-default-directory "~/workspace/")
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

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
  :init
  (add-hook 'comint-mode-hook #'buffer-env-update)
  (add-hook 'hack-local-variables-hook #'buffer-env-update)
  (setq buffer-env-script-name '(".envrc" ".venv/bin/activate")))

(use-package reformatter
  :config
  (reformatter-define black
    :program "black"
    :args '("-"))
  (add-hook 'python-base-mode-hook #'black-on-save-mode))

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(use-package tempel
  :bind (("M-*" . tempel-insert)
         ("M-+" . tempel-complete)))

;;;; python
(setopt python-shell-dedicated 'project)
(setopt python-indent-guess-indent-offset-verbose nil)

(add-to-list 'python-shell-completion-native-disabled-interpreters "python")

(add-to-list 'treesit-language-source-alist
             '(python . ("https://github.com/tree-sitter/tree-sitter-python")))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(add-hook 'python-base-mode-hook #'eglot-ensure)

;;;; R / Julia
(use-package ess
  :defer 5
  :custom
  (ess-use-company nil)
  (ess-ask-for-ess-directory nil)
  (inferior-R-args "--quiet --no-save --no-restore")
  :config
  (require 'ess-site)
  (require 'ess-julia)
  (keymap-set ess-r-mode-map ";" 'ess-insert-assign)
  (keymap-set inferior-ess-r-mode-map ";" 'ess-insert-assign)
  (add-hook 'ess-r-mode-hook #'eglot-ensure))

(use-package julia-mode)

(provide 'init-dev+)
;;; init-dev+.el ends here
