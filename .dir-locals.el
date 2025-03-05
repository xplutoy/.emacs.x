;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((eval . (add-hook 'before-save-hook #'whitespace-cleanup nil 'local)))))
