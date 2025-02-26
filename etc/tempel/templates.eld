fundamental-mode

(today (format-time-string "%Y-%m-%d"))
(now (format-time-string "<%Y-%0m-%0d %a %H:%0M>"))

emacs-lisp-mode

(atload & ";;;###autoload")
(cnst "(defconst " p "\n  \"" p "\")")
(cstom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(grp "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(mcr "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(fn "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflt "(if-let* (" p ")" n> r> ")")
(wnlt "(when-let* (" p ")" n> r> ")")
(adlt "(and-let* (" p ")" n> r> ")")
(wllt "(while-let (" p ")" n> r> ")")
(cnd "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(lt "(let (" p ")" n> r> ")")
(lt* "(let* (" p ")" n> r> ")")
(rc "(letrec (" p ")" n> r> ")")
(dlist "(dolist (" p ")" n> r> ")")
(lp "(cl-loop for " p " in " p " do" n> r> ")")


org-mode

(lnk & "[[" p "][" p "]]")
(inlsrc "src_" p "{" q "}")
(nm & "#+name: " p n)
(cn & "#+caption: ")
(el & "#+begin_src emacs-lisp" n r n "#+end_src" :post (org-edit-src-code))
(py & "#+begin_src python" n r n "#+end_src" :post (org-edit-src-code))
(R  & "#+begin_src R" n r n "#+end_src" :post (org-edit-src-code))
(sh & "#+begin_src shell" n r n "#+end_src" :post (org-edit-src-code))
(lh & "#+latex_header: \\usepackage{wrapfig}" n
    "#+latex_header: \\usepackage{amsthm}" n
    "#+latex_header: \\theoremstyle{definition}" n
    "#+latex_header: \\newtheorem{definition}{定义}" n
    "#+latex_header: \\theoremstyle{remark}" n
    "#+latex_header: \\newtheorem{remark}{备注}" n
    "#+latex_header: \\theoremstyle{plain}" n
    "#+latex_header: \\newtheorem{theorem}{定理}[section]" n
    "#+latex_header: \\newtheorem{lemma}[theorem]{引理}" n)

python-base-mode

(for "for " p " in " p ":" n> q)
(from "from " p " import " q)
(if "if " p ":" n> q)
(ife "if " p ":" n> p n> "else:" n> q)
(ifm "if __name__ == '__main__':" n> q)
(init "def __init__(self" p "):" n> q)
(static "@staticmethod" n> "def " p "(" p "):" n> q)
(classmethod "@classmethod" n> "def " p "(cls, " p "):" n> q)
(not_impl "raise NotImplementedError")
(try "try:" n> p n> "except " p ":" n> q)
(tryelse "try:" n> p n> "except " p ":" n> p n> "else:" n> q)
(np "import numpy as np" n> q)
(pd "import pandas as pd" n> q)
(plt "import matplotlib.pyplot as plt" n> q)
(__eq "def __eq__(self, other):" n> "return self." p " == other." q)
(__getitem "def __len__(self):" n> p n> "pass")
(__iter "def __iter__(self):" n> "return " q)
