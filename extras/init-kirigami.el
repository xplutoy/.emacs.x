;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2026-08-29 17:29:39

;;; Commentary:

;;

;;; Code:

(use-package kirigami
  :custom
  (kirigami-show-menu-bar t)
  (kirigami-show-context-menu t)
  :init
  (kirigami-global-mode 1))

(keymap-global-set "C-c z o" #'kirigami-open-fold)
(keymap-global-set "C-c z O" #'kirigami-open-fold-rec)
(keymap-global-set "C-c z r" #'kirigami-open-folds)
(keymap-global-set "C-c z c" #'kirigami-close-fold)
(keymap-global-set "C-c z m" #'kirigami-close-folds)
(keymap-global-set "C-c z a" #'kirigami-toggle-fold)


(provide 'init-kirigami)
;;; init-kirigami.el ends here
