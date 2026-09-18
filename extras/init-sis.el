;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2026-05-10 10:27:59

;;; Commentary:

(use-package sis
  :unless yx/winp
  :demand
  :config
  (add-to-list 'sis-prefix-override-keys "M-s")
  (add-to-list 'sis-prefix-override-keys "M-g")
  (if yx/macp
      (sis-ism-lazyman-config "com.apple.keylayout.ABC"
			      "com.apple.inputmethod.SCIM.Shuangpin")
    (sis-ism-lazyman-config "1" "2" 'fcitx5))
  (sis-global-inline-mode +1)
  (sis-global-respect-mode +1))

;;; Code:

(provide 'init-sis)
;;; init-sis.el ends here
