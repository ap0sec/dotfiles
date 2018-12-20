(require 'ox-latex)
(require 'ox-bibtex)

;;; LaTeX 形式のファイル PDF に変換するためのコマンド
(setq org-latex-pdf-process
      '("uplatex %f"
        "uplatex %f"
        "ubibtex %b"
        "uplatex %f"
        "uplatex %f"
        "dvipdfmx %b.dvi"))

;;; \hypersetup{...} を出力しない
(setq org-latex-with-hyperref nil)


(add-to-list 'org-latex-classes
             '("jreport"
               "\\documentclass{jreport}
                [NO-PACKAGES]
                [NO-DEFAULT-PACKAGES]
                \\usepackage[dvipdfmx]{graphicx}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("jsarticle"
               "\\documentclass[a4paper,titlepage]{jsarticle}
                [NO-PACKAGES]
                [NO-DEFAULT-PACKAGES]
                \\usepackage{caption}
                \\usepackage{url}
                \\usepackage[dvipdfmx]{graphicx}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")               
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("mini_report"
               "\\documentclass[a4paper,titlepage]{jsarticle}
                [NO-PACKAGES]
                [NO-DEFAULT-PACKAGES]
                \\usepackage{caption}
                \\usepackage{url}
                \\usepackage[dvipdfmx]{graphicx}
                \\usepackage{titlesec}"
               ("\\section*{%s}" . "\\section*{%s}")               
               ("\\subsection*{%s}" . "\\subsection*{%s}")
               ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph*{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph*{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("kurimoto"
               "\\documentclass[12pt]{jarticle}
                [NO-PACKAGES]
                [NO-DEFAULT-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")               
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("proceeding_DJ"
               "\\documentclass[twocolumn, fleqn, uplatex]{jsarticle}
                [NO-PACKAGES]
                [NO-DEFAULT-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")               
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
