;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
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


(defun yx/keyboard-quit ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'."
  (interactive)
  (cond ((> (minibuffer-depth) 0)
         (abort-recursive-edit))
        (t
         (keyboard-quit))))

(keymap-global-set "C-g" #'yx/keyboard-quit)


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
        (setq org-inline-image-overlays (delete ov org-inline-image-overlays)))
      )))


(provide 'init-util)
;;; init-util.el ends here

