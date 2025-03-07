;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-07 14:21:27

;;; Commentary:

;;

;;; Code:

(use-package dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
	 :map dired-mode-map
	 ([remap dired-do-async-shell-command] . dwim-shell-command)
	 ([remap dired-do-shell-command] . dwim-shell-command)
	 ([remap dired-smart-shell-command] . dwim-shell-command)))


(provide 'init-dwim-shell-command)
;;; init-dwim-shell-command.el ends here
