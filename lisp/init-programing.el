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

(with-eval-after-load 'eshell
  (defun yx/eshell-init-h ()
  (eshell-hist-mode +1)
  (keymap-local-set "C-l" #'eshell/clear))
  (add-hook 'eshell-mode-hook #'yx/eshell-init-h)
  (push 'eshell-rebind eshell-modules-list))

(with-eval-after-load 'comint
  (keymap-set comint-mode-map "S-<return>" #'comint-accumulate))

(define-skeleton yx/el-header  ""
  nil
  ";;; -*- lexical-binding: t -*-" \n \n
  ";; Author:  " (user-full-name) \n
  ";; Created: " (format-time-string "%F %T") \n \n
  ";;; Commentary:\n\n;; " @ _ \n \n
  ";;; Code:" \n \n
  "(provide '" (file-name-base (buffer-file-name)) ")" \n
  ";;; " (file-name-nondirectory (buffer-file-name)) " ends here")

(define-auto-insert "\\.el$" #'yx/el-header)

(with-eval-after-load 'eglot
  (fset #'jsonrpc--log-event #'ignore)
  (keymap-set eglot-mode-map "C-c l r" #'eglot-rename)
  (keymap-set eglot-mode-map "C-c l f" #'eglot-format)
  (keymap-set eglot-mode-map "C-c l a" #'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c l h" #'eglot-help-at-point))

(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-g p" #'flymake-goto-prev-error)
  (keymap-set flymake-mode-map "M-g n" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "C-c l d" #'flymake-show-buffer-diagnostics))

(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
	       '(python . ("https://github.com/tree-sitter/tree-sitter-python")))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

;;; Ide

(use-package magit
  :bind (("C-c g" . magit-dispatch)
	 ("C-c v" . magit-file-dispatch))
  :custom
  (magit-clone-default-directory "~/workspace/")
  (magit-save-repository-buffers 'dontask)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package buffer-env
  :unless IS-WIN
  :hook ((comint-mode . buffer-env-update)
	 (hack-local-variables . buffer-env-update))
  :custom
  (buffer-env-script-name '(".envrc" ".venv/bin/activate")))

(use-package reformatter
  :config
  (reformatter-define py-black :program "black" :args '("-")))

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

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

(use-package citre
  :hook (prog-mode . citre-auto-enable-citre-mode)
  :bind (("C-x c ." . citre-jump)
	 ("C-x c ," . citre-jump-back)
	 ("C-x c ;" . citre-peek)
	 ("C-x c /" . citre-jump-to-reference)
	 ("C-x c u" . citre-update-this-tags-file))
  :custom
  (citre-edit-ctags-options-manually nil)
  (citre-default-create-tags-file-location 'global-cache)
  :config
  (when (display-graphic-p)
    (setq citre-peek-fill-fringe nil)
    (setq citre-peek-use-dashes-as-horizontal-border t))
  (keymap-set citre-peek-keymap "C-g" #'citre-peek-abort))

;;; Langs

(with-eval-after-load 'python
  (add-to-list 'project-vc-extra-root-markers "pyproject.toml")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python"))

(add-hook 'python-mode-hook #'eglot-ensure)

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

(use-package julia-mode)

(provide 'init-programing)
;;; init-programing.el ends here
