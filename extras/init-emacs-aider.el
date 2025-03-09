;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-09 08:44:44

;;; Commentary:

;;

;;; Code:

(use-package emacs-aider
  :vc (:url "https://github.com/xplutoy/emacs-aider.el" :branch "main")
  :hook (after-init . global-emacs-aider-mode))

(provide 'init-emacs-aider)
;;; init-emacs-aider.el ends here
