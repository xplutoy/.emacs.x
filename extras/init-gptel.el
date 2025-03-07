;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-07 21:29:09

;;; Commentary:

;;

;;; Code:

(use-package gptel
  :bind (("C-c <return>" . gptel-send)
	 ("C-c C-<return>" . gptel-menu))
  :custom
  (gptel-model 'deepseek-chat)
  (gptel-backend (gptel-make-openai "DeepSeek"
		   :host "api.deepseek.com"
		   :key 'gptel-api-key
		   :endpoint "/chat/completions"
		   :models '(deepseek-chat deepseek-coder)))
  (gptel-default-mode 'org-mode)
  :config

  (when IS-WIN
    (setopt gptel-use-curl nil))
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)

  (defun yx/gptel-quick--callback (resp info)
    (if (length< resp 20)
	(progn
	  (kill-new resp)
	  (message "LLM^^: %s"  resp))
      (with-current-buffer (get-buffer-create "*gptel-quick*")
	(erase-buffer)
	(insert resp)
	(display-buffer (current-buffer)))))

  (defun yx/gptel-quick (capture-text)
    "Explain or summarize region or thing at point with an LLM."
    (interactive
     (list (cond
	    ((use-region-p) (buffer-substring-no-properties (region-beginning)
							    (region-end)))
	    ((derived-mode-p 'prog-mode) (thing-at-point 'defun t))
	    (t (thing-at-point 'sentence t)))))
    (require 'gptel)
    (let* ((default-prompt  "请用精炼的语言解释或总结。")
	   (query-text (concat "###\n\n" capture-text
			       "###\n\n" (if current-prefix-arg
					     (read-string "LLM><：")
					   default-prompt)))
	   (gptel-use-context nil))
      (gptel-request query-text
		     :system "你是一位生活在 Emacs 中专业的编码和写作助手，善于分析代码和总结文章。"
		     :callback #'yx/gptel-quick--callback)))

  (keymap-global-set "M-s q" #'yx/gptel-quick))


(provide 'init-gptel)
;;; init-gptel.el ends here
