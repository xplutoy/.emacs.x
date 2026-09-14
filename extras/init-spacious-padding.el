;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-30 01:54:39

;;; Commentary:

;;

;;; Code:

(use-package spacious-padding
  :custom
  (spacious-padding-widths '( :mode-line-width 1
			      :header-line-width 1
			      :tab-width 0
			      :right-divider-width 20
			      :scroll-bar-width 8
			      :custom-button-width 2
			      :fringe-width 8
			      :internal-border-width 12)))

(spacious-padding-mode +1)

(provide 'init-spacious-padding)
;;; init-spacious-padding.el ends here
