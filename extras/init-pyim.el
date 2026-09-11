;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2026-09-11 09:00:01

;;; Commentary:

;;

;;; Code:

(use-package pyim
  :init
  (setq default-input-method "pyim")
  (pyim-default-scheme 'xiaohe-shuangpin)
  (setq pyim-punctuation-translate-p '(no))
  (setq pyim-english-input-switch-functions '(pyim-probe-org-speed-commands
					      pyim-probe-org-structure-template)))

(provide 'init-pyim)
;;; init-pyim.el ends here
