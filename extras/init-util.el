;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 11:18:22
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(defvar my-http-proxy "127.0.0.1:7890")
(defvar my-socks-proxy "127.0.0.1:7890")

(defun yx/toggle-http-proxy ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (not (bound-and-true-p url-proxy-services))
      (progn
	(setq url-proxy-services
              `(("http" . ,my-http-proxy)
		("https" . ,my-http-proxy)
		("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
	(message "Current HTTP proxy is `%s'" my-http-proxy))
    (setq url-proxy-services nil)
    (message "No HTTP proxy")))

(defun yx/toggle-socks-proxy ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (not (bound-and-true-p socks-server))
      (let* ((proxy (split-string my-socks-proxy ":"))
             (host (car proxy))
             (port (string-to-number (cadr proxy))))
        (setq socks-server `("Default server" ,host ,port 5)
              url-gateway-method 'socks
              socks-noproxy '("localhost"))
        (setenv "all_proxy" (concat "socks5://" my-socks-proxy))
        (message "Current SOCKS%d proxy is %s:%s"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server)))
    (setq url-gateway-method 'native
          socks-noproxy nil
          socks-server nil)
    (message "No SOCKS proxy")))

(add-hook 'emacs-startup-hook #'yx/toggle-http-proxy)
(add-hook 'emacs-startup-hook #'yx/toggle-socks-proxy)

(defun yx/keyboard-quit ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'."
  (interactive)
  (cond ((> (minibuffer-depth) 0)
         (abort-recursive-edit))
        (t
         (keyboard-quit))))

(defun yx/org-toggle-inline-images-in-subtree (state &optional beg end)
  "Refresh inline image previews in the current heading/tree."
  (let* ((beg (or beg
                  (if (org-before-first-heading-p)
                      (save-excursion (point-min))
                    (save-excursion (org-back-to-heading) (point)))))
         (end (or end
                  (if (org-before-first-heading-p)
                      (save-excursion (org-next-visible-heading 1) (point))
                    (save-excursion (org-end-of-subtree) (point)))))
         (overlays (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-image-overlay))
                                     (ignore-errors (overlays-in beg end)))))
    (if (and (eq state 'subtree) (not overlays))
        (org-display-inline-images t t beg end)
      (dolist (ov overlays nil)
        (delete-overlay ov)
        (setq org-inline-image-overlays (delete ov org-inline-image-overlays))))))

(defun yx/auth-get-pwd (host)
  "Find `secret' in `auth-sources' for HOST entry."
  (when-let ((source (auth-source-search :host host)))
    (funcall (plist-get (car source) :secret))))


(provide 'init-util)
;;; init-util.el ends here

