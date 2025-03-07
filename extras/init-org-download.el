;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-07 14:05:48

;;; Commentary:

;;

;;; Code:

(use-package org-download
  :config
  (setq org-download-heading-lvl nil)
  (setq org-download-image-dir (expand-file-name "images" my-org-attach-dir))
  (setq org-download-annotate-function (lambda (_link) ""))
  (add-hook 'dired-mode-hook 'org-download-enable))

(provide 'init-org-download)
;;; init-org-download.el ends here
