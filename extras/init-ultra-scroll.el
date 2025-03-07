;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-07 14:02:51

;;; Commentary:

;;

;;; Code:

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll" :branch "main" :rev :newest)
  :hook (after-init . ultra-scroll-mode))


(provide 'init-ultra-scroll)
;;; init-ultra-scroll.el ends here
