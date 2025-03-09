;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-07 22:36:59

;;; Commentary:

;;

;;; Code:

(use-package dirvish
  :init
  (dirvish-override-dired-mode +1)
  :custom
  (dirvish-default-layout '(0 0.4 0.6))
  (dirvish-side-auto-close t)
  (dirvish-attributes '(file-time file-size collapse subtree-state))
  (dirvish-side-attributes '(vc-state file-size collapse))
  (dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-quick-access-entries '(("h" "~/" "Home")  ; `setq' won't work
				  ("d" "~/workspace/" "Workspace")
				  ("n" "~/org-notes/" "Notes")))
  :config
  ;; (dirvish-peek-mode +1)
  (dirvish-side-follow-mode +1)

  (defun yx/dirvish-modeline-height-init-h ()
    "Initialize dirvish modeline and headerline height."
    (let ((height (window-mode-line-height)))
      (setq dirvish-mode-line-height height)
      (setq dirvish-header-line-height height)))

  (yx/dirvish-modeline-height-init-h)

  :bind (("C-c f" . dirvish-side)
	 :map dirvish-mode-map
	 ("?"   . dirvish-dispatch)
	 ("a"   . dirvish-quick-access)
	 ("f"   . dirvish-file-info-menu)
	 ("y"   . dirvish-yank-menu)
	 ("N"   . dirvish-narrow)
	 ("h"   . dirvish-history-jump)
	 ("s"   . dirvish-quicksort)
	 ("v"   . dirvish-vc-menu)
	 ("TAB" . dirvish-subtree-toggle)
	 ("M-f" . dirvish-history-go-forward)
	 ("M-b" . dirvish-history-go-backward)
	 ("M-l" . dirvish-ls-switches-menu)
	 ("M-m" . dirvish-mark-menu)
	 ("M-t" . dirvish-layout-toggle)
	 ("M-s" . dirvish-setup-menu)
	 ("M-j" . dirvish-fd-jump)))


(provide 'init-dirvish)
;;; init-dirvish.el ends here
