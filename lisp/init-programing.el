;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 11:59:57
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(which-function-mode +1)
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
  (setopt treesit-language-source-alist
	  '((python . ("https://github.com/tree-sitter/tree-sitter-python"))
	    (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
	    (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
	    (json . ("https://github.com/tree-sitter/tree-sitter-json"))
	    (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
	    (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))))
  (setopt major-mode-remap-alist '((c++-mode . c++-ts-mode)
				   (python-mode . python-ts-mode)))
  (add-to-list 'treesit-load-name-override-list '(c++ "libtree-sitter-cpp")))

;;; Ide+

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

;;; Langs

;; python

(with-eval-after-load 'python
  (add-to-list 'project-vc-extra-root-markers "pyproject.toml")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python"))


(provide 'init-programing)
;;; init-programing.el ends here
