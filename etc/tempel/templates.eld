fundamental-mode

(today (format-time-string "%Y-%m-%d"))
(now (format-time-string "<%Y-%0m-%0d %a %H:%0M>"))

;;; prog
prog-mode

(header comment-start " -----------------------------------------------------------" n
	comment-start " Author:      " (user-full-name) n
	comment-start " Date:        " (format-time-string "%F %T") n
	comment-start " Description: " (p "") n
	comment-start " -----------------------------------------------------------" n)


;;; elisp
emacs-lisp-mode

(autoload & ";;;###autoload")

(defgroup "(defgroup " (p (file-name-base (buffer-file-name))) " ()" n>
  "\"" p "\"" n>
  ":group '" (p "") n>
  ":prefix \""  (p (file-name-base (buffer-file-name))) "-\"" ")")

(defcustom "(defcustom " p " " (p "nil") n>
  "\"" p "\"" n>
  ":type '" p ")")

(defface "(defface " p " '((t :inherit " p "))\n  \"" p "\")")


;;; org
org-mode


(inlsrc "src_" p "{" p "}")

(gnuplot "#+begin_src gnuplot :var data=" (p "table") " :file " (p "plot.png")
	 n r n> "#+end_src")

(ctexart & "#+LATEX_COMPILER: xelatex" n
	 "#+LATEX_CLASS: ctexart" n>
	 "#+LATEX_CLASS_OPTIONS: [a4paper,11pt]" n>
	 "#+LATEX_HEADER: \\usepackage{amsthm}" n>
	 "#+LATEX_HEADER: \\theoremstyle{definition}" n>
	 "#+LATEX_HEADER: \\newtheorem{definition}{定义}" n>
	 "#+LATEX_HEADER: \\theoremstyle{remark}" n>
	 "#+LATEX_HEADER: \\newtheorem{remark}{备注}" n>
	 "#+LATEX_HEADER: \\theoremstyle{plain}" n>
	 "#+LATEX_HEADER: \\newtheorem{theorem}{定理}[section]" n>
	 "#+LATEX_HEADER: \\newtheorem{lemma}[theorem]{引理}" n)


;;; python
python-mode

(inp  "import numpy as np" n>)
(ipd  "import pandas as pd" n>)
(iplt "import matplotlib.pyplot as plt" n>)

(main "if __name__ == '__main__':" n>)
(script "#!/usr/bin/env python" n n> "def main():" n> "pass" n n> "if __name__ == '__main__':" n> "main()")

