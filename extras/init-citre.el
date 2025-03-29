;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-30 01:30:24

;;; Commentary:

;;

;;; Code:

(use-package citre
  :hook ((prog-mode . citre-auto-enable-citre-mode))
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

(provide 'init-citre)
;;; init-citre.el ends here
