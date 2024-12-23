;;; -*- lexical-binding: t -*-

;; Author:  yangxue <yangxue.cs@foxmail.com>
;; Created: 2024-07-05 11:18:22
;; License: GPLv3

;;; Commentary:

;;

;;; Code:

(defun yx/auth-get-pwd (host)
  "Find `secret' in `auth-sources' for HOST entry."
  (when-let ((source (auth-source-search :host host)))
    (funcall (plist-get (car source) :secret))))


(defun yx/keyboard-quit ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'."
  (interactive)
  (cond ((> (minibuffer-depth) 0)
         (abort-recursive-edit))
        (t
         (keyboard-quit))))

(keymap-global-set "C-g" #'yx/keyboard-quit)


(defun yx/quick-window-jump ()
  "My DWIM window jumping.

If there is only one windows, switch to the other buffer.
If there are only two windows, jump directly to the other window.
Otherwise jump to a window by typing its assigned character label."
  (interactive)
  (let* ((window-lst (window-list nil 'no-mini))
	 (window-num (length window-lst)))
    (cond ((= window-num 1)
	   (switch-to-buffer (other-buffer)))
	  ((= window-num 2)
	   (select-window (other-window-for-scrolling)))
	  (t
	   (let* ((my-quick-window-overlays nil)
		  (sorted-windows (sort window-lst
					(lambda (w1 w2)
					  (let ((edges1 (window-edges w1))
						(edges2 (window-edges w2)))
					    (or (< (car edges1) (car edges2))
						(and (= (car edges1) (car edges2))
                                                     (< (cadr edges1) (cadr edges2))))))))
		  (window-keys (seq-take '("j" "k" "l" ";" "a" "s" "d" "f") window-num))
		  (window-map (cl-pairlis window-keys sorted-windows)))
             (setq my-quick-window-overlays
		   (mapcar (lambda (entry)
                             (let* ((key (car entry))
				    (window (cdr entry))
				    (start (window-start window))
				    (overlay (make-overlay start start (window-buffer window))))
                               (overlay-put overlay 'after-string
					    (propertize (format "[%s]" key)
							'face '(:foreground "white" :background "blue" :weight bold)))
                               (overlay-put overlay 'window window)
                               overlay))
			   window-map))
             (let ((key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
               (mapc #'delete-overlay my/quick-window-overlays)
               (setq my-quick-window-overlays nil)
               (when-let ((selected-window (cdr (assoc (char-to-string key) window-map))))
		 (select-window selected-window))))))))

(keymap-global-set "M-o" #'yx/quick-window-jump)


(provide 'init-util)
;;; init-util.el ends here

