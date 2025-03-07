;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-07 13:54:06

;;; Commentary:

;;

;;; Code:

(use-package color-rg
  :vc (:url "https://github.com/xplutoy/color-rg" :branch "master")
  :bind (("M-s s" . color-rg-search-symbol-in-current-file)
	 ("M-s M-s" . color-rg-search-input-in-current-file)
	 ("M-s p" . color-rg-search-symbol-in-project)
	 ("M-s M-p" . color-rg-search-input-in-project))
  :init
  (setopt color-rg-search-no-ignore-file nil)
  (setopt color-rg-mac-load-path-from-shell nil)
  (when IS-WIN (setopt color-rg-command-prefix "powershell")))


(provide 'init-color-rg)
;;; init-color-rg.el ends here
