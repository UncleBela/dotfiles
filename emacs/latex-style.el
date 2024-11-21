;; my-latex-style.el

;; Custom LaTeX preamble for Org mode export
(setq org-latex-classes
      '(("article"
         "\\documentclass[15pt]{article}
          [PACKAGES]
          [EXTRA]
          \\usepackage{amsmath}
          \\usepackage{amssymb}
          \\usepackage{hyperref}
          \\usepackage{graphicx}
          \\usepackage{xcolor}
          \\usepackage{geometry}
          \\geometry{a4paper, margin=1in}
          % Add more custom packages or commands here
          \\newcommand{\\mycustomcommand}{Your Custom Command}

          \\hypersetup{
            colorlinks=true,
            linkcolor=blue,
            urlcolor=cyan,
            pdftitle={My PDF},
            pdfpagemode=FullScreen,
          }"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Ensure to set this custom preamble in `org-latex-pdf-process` if needed
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))
