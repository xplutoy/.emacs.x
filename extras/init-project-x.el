;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2026-09-18 09:24:35

;;; Commentary:

;;

;;; Code:

(use-package project-x
  :hook ((after-init . project-x-mode)
	 (project-x-mode . project-x-tabs-mode))
  :custom
  (project-x-auto-save-delay 5)
  (project-prompter #'project-x--project-prompt)
  (project-x-restore-last-project-on-startup t))

(provide 'init-project-x)
;;; init-project-x.el ends here
