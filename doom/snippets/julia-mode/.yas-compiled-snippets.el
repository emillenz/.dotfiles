;;; Compiled snippets and support files for `julia-mode'
;;; contents of the .yas-setup.el support file:
;;;
;;; julia-mode/.yas-setup.el -*- lexical-binding: t; -*-

(defun yas-julia-doc-args ()
  "Format arguments of a function slightly nicer for the doc string"
  (replace-regexp-in-string "\\([:blank:]*[,;]?*[^,;=]+=[[:ascii:][:nonascii:]]+\\)" "[\\1]" yas-text))
;;; Snippet definitions:
;;;
(yas-define-snippets 'julia-mode
                     '(("while" "while ${1:cond}\n    `%`${2:body}\nend\n$0" "while ... ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/while" nil nil)
                       ("try" "try\n    `%`${1:expr}\ncatch ${2:error}\n    ${3:e_expr}\nfinally\n    ${4:f_expr}\nend\n$0\n" "try ... catch ... finally ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/tryf" nil nil)
                       ("try" "try\n    `%`${1:expr}\ncatch ${2:error}\n    ${3:e_expr}\nend\n$0\n" "try ... catch ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/try" nil nil)
                       ("struct" "struct ${1:name}\n    `%`$0\nend" "struct ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/struct" nil nil)
                       ("quote" "quote\n    `%`$0\nend" "quote ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/quote" nil nil)
                       ("ptype" "primitive type ${1:${2:type} <: ${3:supertype}} ${4:bits} end\n$0\n" "primitive type ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/ptype" nil nil)
                       ("mutstr" "mutable struct ${1:name}\n    `%`$0\nend" "mutable struct ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/mutstr" nil nil)
                       ("module" "module ${1:name}\n`%`$0\nend\n" "module ... ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/module" nil nil)
                       ("macro" "macro ${1:macro}(${2:args})\n    `%`$0\nend\n" "macro(...) ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/macro" nil nil)
                       ("let" "let ${1:binding}\n    `%`$0\nend" "let ... ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/let" nil nil)
                       ("ife" "if ${1:cond}\n    `%`${2:true}\nelse\n    ${3:false}\nend\n$0" "if ... ... else ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/ife" nil nil)
                       ("if" "if ${1:cond}\n    `%`$0\nend\n" "if ... ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/if" nil nil)
                       ("fun" "function ${1:name}(${2:args})\n    `%`$0\nend" "function(...) ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/fun" nil nil)
                       ("for" "for ${1:i} in ${2:1:n}\n    `%`$0\nend\n" "for ... ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/for" nil nil)
                       ("do" "do ${1:x}\n    `%`$0\nend" "do ... ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/do" nil nil)
                       ("dfun" "@doc raw\"\"\"\n    $1(${2:$(yas-julia-doc-args)})\n\n${3:Documentation of function.}\n\n# Examples\n\\`\\`\\`jldoctest\njulia> $1($4)\ninsert result of $1($4)\n\\`\\`\\`\n\"\"\"\nfunction ${1:name}(${2:args})\n    `%`$0\nend\n" "@doc ... function ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/dfun" nil "dfun")
                       ("begin" "begin\n    `%`$0\nend\n" "begin ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/begin" nil nil)
                       ("beg"
                        (progn
                          (doom-snippets-expand :name "begin"))
                        "begin" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/beg" nil "beg")
                       ("atype" "abstract type ${1:${2:type} <: ${3:supertype}} end\n$0" "abstract type ... end" nil nil nil "/home/lenz/.config/doom/snippets/julia-mode/atype" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:48 2024
