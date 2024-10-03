;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
                     '(("pkg" "\\usepackage{$0}" "usepackage" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/usepackage" nil "pkg")
                       ("thm" "\\begin{theorem}\n`%`$0\n\\end{theorem}" "theorem" nil
                        ("theorems")
                        nil "/home/lenz/.config/doom/snippets/latex-mode/theorem" nil nil)
                       ("b" "\\textbf{`%`$1}$0" "textbf"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/lenz/.config/doom/snippets/latex-mode/textbf" nil "b")
                       ("sum" "\\sum${1:$(when (> (length yas-text) 0) \"_\")\n}${1:$(when (> (length yas-text) 1) \"{\")\n}${1:i=0}${1:$(when (> (length yas-text) 1) \"}\")\n}${2:$(when (> (length yas-text) 0) \"^\")\n}${2:$(when (> (length yas-text) 1) \"{\")\n}${2:n}${2:$(when (> (length yas-text) 1) \"}\")} $0" "sum_^" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/sum_^" nil nil)
                       ("sub" "\\subsection{${1:name}}%\n\\label{subsec:${2:label}}\n\n$0" "subsec" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/subsec" nil "sub")
                       ("subfig" "\\begin{figure}[ht]\n  \\centering\n  \\subfigure[$1]\n  {\\label{fig:${2:label}} \n    \\includegraphics[width=.${3:5}\\textwidth]{${4:path}}}\n\n  \\caption{${5:caption}}%\n\\label{fig:${6:label}}\n\\end{figure}\n" "subfigure" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/subfigure" nil "subfig")
                       ("sf" "\\subfigure[${1:caption}]{\n  \\label{fig:${2:label}} \n  \\includegraphics[width=.${3:3}\\textwidth]{${4:path}}}\n$0" "subf" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/subf" nil "sf")
                       ("sq" "\\sqrt{`%`$1}$0" "sqrt" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/sqrt" nil nil)
                       ("sec" "\\section{${1:name}}%\n\\label{sec:${2:label}}\n\n$0" "section" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/section" nil "sec")
                       ("root" "\\sqrt[$1]{`%`$2}" "sqrt[]{}" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/root" nil nil)
                       ("rmk" "\\begin{remark}\n`%`$0\n\\end{remark}" "remark" nil
                        ("theorems")
                        nil "/home/lenz/.config/doom/snippets/latex-mode/remark" nil nil)
                       ("q" "\\question{${0:`%`}}" "question"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/lenz/.config/doom/snippets/latex-mode/question" nil "q")
                       ("py" "\\lstset{language=python}\n\\begin[language=python]{lstlisting}\n${0:`%`}\n\\end{lstlisting}" "python" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/python" nil "py")
                       ("prf" "\\begin{proof}\n`%`$0\n\\end{proof}" "proof" nil
                        ("theorems")
                        nil "/home/lenz/.config/doom/snippets/latex-mode/proof" nil nil)
                       ("prod" "\\prod${1:$(when (> (length yas-text) 0) \"_\")\n}${1:$(when (> (length yas-text) 1) \"{\")\n}${1:i=0}${1:$(when (> (length yas-text) 1) \"}\")\n}${2:$(when (> (length yas-text) 0) \"^\")\n}${2:$(when (> (length yas-text) 1) \"{\")\n}${2:n}${2:$(when (> (length yas-text) 1) \"}\")} $0" "prod_^" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/prod_^" nil nil)
                       ("no" "\\note{${0:`%`}}" "note" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/note" nil "no")
                       ("gl" "\\newglossaryentry{${1:AC}}{name=${2:Andrea Crotti}${3:, description=${4:description}}}" "newglossaryentry" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/newglossaryentry" nil "gl")
                       ("cmd" "\\newcommand{\\\\${1:name}}${2:[${3:0}]}{$0}" "newcommand" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/newcommand" nil "cmd")
                       ("movie" "\\begin{center}\n\\includemovie[\n  label=test,\n  controls=false,\n  text={\\includegraphics[width=4in]{${1:image.pdf}}}\n]{4in}{4in}{${2:video file}}\n\n\\movieref[rate=3]{test}{Play Fast}\n\\movieref[rate=1]{test}{Play Normal Speed} \n\\movieref[rate=0.2]{test}{Play Slow}\n\\movieref[resume]{test}{Pause/Resume}\n" "movie" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/movie" nil "movie")
                       ("mc" "\\mathclap{`%`$0}" "mathclap" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/mathclap" nil nil)
                       ("lst" "\\begin{lstlisting}[float,label=lst:${1:label},caption=nextHopInfo: ${2:caption}]\n${0:`%`}\n\\end{lstlisting}" "listing" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/listing" nil "lst")
                       ("limsup" "\\limsup_{${1:n} \\to ${2:\\infty}} $0" "limsup" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/limsup" nil nil)
                       ("liminf" "\\liminf_{${1:n} \\to ${2:\\infty}} $0" "liminf" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/liminf" nil nil)
                       ("lim" "\\lim_{${1:n} \\to ${2:\\infty}} $0" "lim" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/lim" nil nil)
                       ("lmm" "\\begin{lemma}\n`%`$0\n\\end{lemma}" "lemma" nil
                        ("theorems")
                        nil "/home/lenz/.config/doom/snippets/latex-mode/lemma" nil nil)
                       ("lab" "\\label{$0}" "label" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/label" nil "lab")
                       ("it" "\\begin{itemize}\n${0:`%`}\n\\end{itemize}" "itemize" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/itemize" nil "it")
                       ("-" "\\item ${0:`%`}" "item"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/lenz/.config/doom/snippets/latex-mode/item" nil "-")
                       ("int" "\\int${1:$(when (> (length yas-text) 0) \"_\")\n}${1:$(when (> (length yas-text) 1) \"{\")\n}${1:left}${1:$(when (> (length yas-text) 1) \"}\")\n}${2:$(when (> (length yas-text) 0) \"^\")\n}${2:$(when (> (length yas-text) 1) \"{\")\n}${2:right}${2:$(when (> (length yas-text) 1) \"}\")} $0" "int_^" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/int_^" nil nil)
                       ("ig" "\\includegraphics${1:[$2]}{$0}" "includegraphics" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/includegraphics" nil "ig")
                       ("if" "\\IF {$${1:cond}$}\n    `%`$0\n\\ELSE\n\\ENDIF \n" "if" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/if" nil "if")
                       ("gp" "\\glspl{${1:label}}" "glspl" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/glspl" nil "gp")
                       ("g" "\\gls{${1:label}}" "gls"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/lenz/.config/doom/snippets/latex-mode/gls" nil "g")
                       ("fr" "\\begin{frame}${1:[$2]}\n        ${3:\\frametitle{$4}}\n        ${0:`%`}\n\\end{frame}" "frame" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/frame" nil "fr")
                       ("frac" "\\frac{${1:`(or % \"numerator\")`}}{${2:denominator}}$0" "frac" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/frac" nil "frac")
                       ("fig" "\\begin{figure}[ht]\n  \\centering\n  \\includegraphics[${1:options}]{figures/${2:path.pdf}}\n  \\caption{\\label{fig:${3:label}} $0}\n\\end{figure}\n" "figure" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/figure" nil "fig")
                       ("exc" "\\begin{exercise}\n`%`$0\n\\end{exercise}" "exercise" nil
                        ("theorems")
                        nil "/home/lenz/.config/doom/snippets/latex-mode/exercise" nil nil)
                       ("en" "\\begin{enumerate}\n${0:`%`}\n\\end{enumerate}\n" "enumerate" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/enumerate" nil "en")
                       ("e" "\\emph{${1:`%`}}$0" "emph"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/lenz/.config/doom/snippets/latex-mode/emph" nil "e")
                       ("def" "\\begin{definition}\n`%`$0\n\\end{definition}" "definition" nil
                        ("theorems")
                        nil "/home/lenz/.config/doom/snippets/latex-mode/definition" nil nil)
                       ("clr" "\\begin{corollary}\n`%`$0\n\\end{corollary}" "corollary" nil
                        ("theorems")
                        nil "/home/lenz/.config/doom/snippets/latex-mode/corollary" nil nil)
                       ("cols" "\\begin{columns}\n  \\begin{column}{.${1:5}\\textwidth}\n  $0\n  \\end{column}\n\n  \\begin{column}{.${2:5}\\textwidth}\n\n  \\end{column}\n\\end{columns}" "columns" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/columns" nil "cols")
                       ("code" "\\begin{lstlisting}\n${0:`%`}\n\\end{lstlisting}" "code" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/code" nil "code")
                       ("c" "\\cite{$1} $0" "cite"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/lenz/.config/doom/snippets/latex-mode/cite" nil "c")
                       ("ca" "\\caption{${0:`%`}}" "caption" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/caption" nil "ca")
                       ("G" "\\Gls{${1:label}}" "Gls"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/lenz/.config/doom/snippets/latex-mode/capgls" nil "G")
                       ("bl" "\\begin{block}{$1}\n        ${0:`%`}\n\\end{block}" "block" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/block" nil "bl")
                       ("cup" "\\bigcup${1:$(when (> (length yas-text) 0) \"_\")\n}${1:$(when (> (length yas-text) 1) \"{\")\n}${1:i=0}${1:$(when (> (length yas-text) 1) \"}\")\n}${2:$(when (> (length yas-text) 0) \"^\")\n}${2:$(when (> (length yas-text) 1) \"{\")\n}${2:n}${2:$(when (> (length yas-text) 1) \"}\")} $0" "bigcup_^" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/bigcup_^" nil nil)
                       ("cap" "\\bigcap${1:$(when (> (length yas-text) 0) \"_\")\n}${1:$(when (> (length yas-text) 1) \"{\")\n}${1:i=0}${1:$(when (> (length yas-text) 1) \"}\")\n}${2:$(when (> (length yas-text) 0) \"^\")\n}${2:$(when (> (length yas-text) 1) \"{\")\n}${2:n}${2:$(when (> (length yas-text) 1) \"}\")} $0" "bigcap_^" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/bigcap_^" nil nil)
                       ("begin" "\\begin{${1:environment}}\n`%`$0\n\\end{$1}" "begin" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/begin" nil "begin")
                       ("axm" "\\begin{axiom}\n`%`$0\n\\end{axiom}" "axiom" nil
                        ("theorems")
                        nil "/home/lenz/.config/doom/snippets/latex-mode/axiom" nil nil)
                       ("alg" "\\begin{algorithmic}\n${0:`%`}\n\\end{algorithmic}\n" "alg" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/alg" nil "alg")
                       ("al" "\\begin{alertblock}{$2}\n        ${0:`%`}\n\\end{alertblock}" "alertblock" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/alertblock" nil "al")
                       ("ac" "\\newacronym{${1:label}}{${1:$(upcase yas-text)}}{${2:Name}}" "acronym" nil nil nil "/home/lenz/.config/doom/snippets/latex-mode/acronym" nil "ac")))


;;; Do not edit! File generated at Thu Apr 25 09:20:48 2024
