;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-07-05 11:18:22
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(defvar yx-proxy "127.0.0.1:7890")

(defun yx/proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (not(bound-and-true-p url-proxy-services))
      (progn
	(setq url-proxy-services
              `(("http" . ,yx-proxy)
		("https" . ,yx-proxy)
		("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
	(message "Current HTTP proxy is `%s'" yx-proxy))
    (setq url-proxy-services nil)
    (message "No HTTP proxy")))

(provide 'init-simple)
;;; init-simple.el ends here

