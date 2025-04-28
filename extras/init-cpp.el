;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-04-28 10:52:50

;;; Commentary:

;;

;;; Code:

(c-set-offset 'innamespace 0)

(add-hook 'c++-mode-hook #'eglot-ensure)

(use-package cmake-mode)


(provide 'init-cpp)
;;; init-cpp.el ends here
