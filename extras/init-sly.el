;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-10 21:31:52

;;; Commentary:

;;

;;; Code:

(use-package sly
  :config
  (sly-setup)

  (when-let* ((sbcl (executable-find "sbcl")))
    (setopt inferior-lisp-program sbcl))

  (defun yx/sly-mrepl-return ()
    "Mimic mordern shell return behavor."
    (interactive)
    (comint-accumulate)
    (indent-for-tab-command))

  (keymap-set sly-mrepl-mode-map "<return>" #'yx/sly-mrepl-return)
  (keymap-set sly-mrepl-mode-map "M-<return>" #'sly-mrepl-return))

(provide 'init-sly)
;;; init-sly.el ends here
