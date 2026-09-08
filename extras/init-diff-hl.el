;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2026-09-08 13:55:11

;;; Commentary:

;;

;;; Code:

(use-package diff-hl
  :init
  (setopt diff-hl-draw-borders nil)
  (setopt diff-hl-update-async 'thread)
  (setopt diff-hl-disable-on-remote t)
  (setopt diff-hl-global-modes '(not image-mode pdf-view-mode))
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1)
  (global-diff-hl-show-hunk-mouse-mode -1)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(provide 'init-diff-hl)
;;; init-diff-hl.el ends here
