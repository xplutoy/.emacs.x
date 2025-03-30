;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-30 22:50:30

;;; Commentary:

;;

;;; Code:

(use-package gcmh
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold #x1000000)) ; 16MB

(gcmh-mode +1)

(provide 'init-gcmh)
;;; init-gcmh.el ends here
