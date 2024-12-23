;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 11:18:22
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(defun yx/keyboard-quit ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'."
  (interactive)
  (cond ((> (minibuffer-depth) 0)
         (abort-recursive-edit))
        (t
         (keyboard-quit))))

(keymap-global-set "C-g" #'yx/keyboard-quit)

(defun yx/auth-get-pwd (host)
  "Find `secret' in `auth-sources' for HOST entry."
  (when-let ((source (auth-source-search :host host)))
    (funcall (plist-get (car source) :secret))))


(provide 'init-util)
;;; init-util.el ends here

