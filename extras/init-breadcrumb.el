;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-04-09 19:33:58

;;; Commentary:

;;

;;; Code:

(use-package breadcrumb)

(add-hook 'prog-mode-hook #'breadcrumb-local-mode)

(provide 'init-breadcrumb)
;;; init-breadcrumb.el ends here
