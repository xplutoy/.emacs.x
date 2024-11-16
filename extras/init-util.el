;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 11:18:22
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(defun yx/proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (let ((url-proxy "127.0.0.1:7890"))
    (if (not(bound-and-true-p url-proxy-services))
        (progn
	  (setq url-proxy-services
                `(("http" . ,url-proxy)
		  ("https" . ,url-proxy)
		  ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
	  (message "Current HTTP proxy is `%s'" url-proxy))
      (setq url-proxy-services nil))))

(add-hook 'emacs-startup-hook #'yx/proxy-http-toggle)

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

(defun yx/eshell-toggle ()
  (interactive)
  (require 'eshell)
  (require 'project)
  (unless (eq major-mode 'eshell-mode)
    (let* ((project (project-current))
           (curr-dir(directory-file-name default-directory))
           (root-dir (if project
                         (file-name-nondirectory (directory-file-name (project-root project)))
                       (file-name-nondirectory curr-dir)))
           (popup-buffer-name (format "*Eshell-popup*: %s" root-dir))
           (popup-win (get-buffer-window popup-buffer-name)))
      (if popup-win
          (if (eq (selected-window) popup-win)
              (ignore-errors (delete-window popup-win))
            (select-window popup-win))
        (let ((eshell-buffer-name popup-buffer-name)
              (display-comint-buffer-action '(display-buffer-at-bottom (inhibit-same-window . nil))))
          (with-current-buffer (eshell)
            (unless (string= curr-dir (directory-file-name default-directory))
              (eshell/cd curr-dir)
              (eshell-send-input))
            (add-hook 'eshell-exit-hook
                      (lambda ()
                        (ignore-errors (delete-window (get-buffer-window popup-buffer-name))))
                      nil t)))))))

(defun yx/set-exec-path-from-shell (&optional pathvar)
  (interactive)
  (let* ((pathvar (or pathvar "PATH"))
         (path-from-shell
          (shell-command-to-string
           (format "/bin/zsh -c -i 'echo -n $%s'" pathvar))))
    (setenv pathvar path-from-shell)
    (when (string-equal pathvar "PATH")
      (setq exec-path (split-string path-from-shell path-separator)))))

(provide 'init-util)
;;; init-util.el ends here

