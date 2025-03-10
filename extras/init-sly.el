;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-10 21:31:52

;;; Commentary:

;;

;;; Code:

(use-package sly
  :config
  (when-let* ((sbcl (executable-find "sbcl")))
    (setopt inferior-lisp-program sbcl))
  (sly-setup))

(provide 'init-sly)
;;; init-sly.el ends here
