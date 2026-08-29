;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2026-08-29 16:58:24

;;; Commentary:

;;

;;; Code:

(use-package shell-pop
  :bind (("C-c t" . shell-pop))
  :custom
  (shell-pop-universal-key "C-c t")
  :config
  (if (fboundp 'eat)
      (setopt shell-pop-shell-type '("eat" "*eat*" (lambda () (eat shell-pop-term-shell))))
    (setopt shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell)))))
  )

(provide 'init-shell-pop)
;;; init-shell-pop.el ends here
