;;; Compiled snippets and support files for `nsis-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'nsis-mode
                     '(("sec" "Section \"${1:Program}\"\n        $0\nSectionEnd" "section" nil nil nil "/home/lenz/.config/doom/snippets/nsis-mode/section" nil "sec")
                       ("out" "outFile \"${1:setup}.exe\"" "outfile" nil nil nil "/home/lenz/.config/doom/snippets/nsis-mode/outfile" nil "out")
                       ("$" "$OUTDIR" "outdir" nil nil nil "/home/lenz/.config/doom/snippets/nsis-mode/outdir" nil "$")
                       ("msg" "MessageBox MB_OK \"${1:hello}\"" "message" nil nil nil "/home/lenz/.config/doom/snippets/nsis-mode/message" nil "msg")
                       ("macro" "!macro ${1:Name} UN\n$0\n\n!macroend" "macro" nil nil nil "/home/lenz/.config/doom/snippets/nsis-mode/macro" nil "macro")
                       ("$" "$INSTDIR" "instdir" nil nil nil "/home/lenz/.config/doom/snippets/nsis-mode/instdir" nil "$")
                       ("im" "!insermacro ${1:Name} ${2:\"args\"}" "insert_macro" nil nil nil "/home/lenz/.config/doom/snippets/nsis-mode/insert_macro" nil "im")
                       ("inc" "!include \"${Library.nsh}\"" "include" nil nil nil "/home/lenz/.config/doom/snippets/nsis-mode/include" nil "inc")
                       ("if" "${IF} ${1:cond}\n      $0\n${ElseIf} ${2:else_cond}\n\n${EndIf}" "if" nil nil nil "/home/lenz/.config/doom/snippets/nsis-mode/if" nil "if")
                       ("fun" "Function ${1:Name}\n         $0\nFunctionEnd" "function" nil nil nil "/home/lenz/.config/doom/snippets/nsis-mode/function" nil "fun")
                       ("def" "!define ${1:CONSTANT} ${2:value}" "define" nil nil nil "/home/lenz/.config/doom/snippets/nsis-mode/define" nil "def")))


;;; Do not edit! File generated at Thu Apr 25 09:20:48 2024
