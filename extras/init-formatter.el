;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-04-01 22:52:13

;;; Commentary:

;;

;;; Code:

(use-package apheleia)

(use-package reformatter)

(reformatter-define black-format
  :program "black" :args '("-q" "-"))

(reformatter-define ruff-format
  :program "ruff"
  :args `("format" "--stdin-filename" ,(or (buffer-file-name) input-file) "-"))


(provide 'init-formatter)
;;; init-formatter.el ends here
