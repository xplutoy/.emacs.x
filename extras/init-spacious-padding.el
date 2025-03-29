;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-30 01:54:39

;;; Commentary:

;;

;;; Code:

(use-package spacious-padding
  :custom
  (spacious-padding-widths '( :header-line-width 4
			      :mode-line-width 3
			      :tab-width 4
			      :right-divider-width 15
			      :scroll-bar-width 8
			      :internal-border-width 15))
  (spacious-padding-subtle-mode-line nil))

(spacious-padding-mode +1)

(provide 'init-spacious-padding)
;;; init-spacious-padding.el ends here
