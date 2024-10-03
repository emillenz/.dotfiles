;;; Compiled snippets and support files for `cc-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'cc-mode
                     '(("while" "while (${1:condition}) {\n    $0\n}" "while" nil nil nil "/home/lenz/.config/doom/snippets/cc-mode/while" nil nil)
                       ("typedef" "typedef ${1:type} ${2:alias};" "typedef" nil nil nil "/home/lenz/.config/doom/snippets/cc-mode/typedef" nil nil)
                       ("?" "(${1:cond}) ? ${2:then} : ${3:else};" "ternary" nil nil nil "/home/lenz/.config/doom/snippets/cc-mode/ternary" nil "?")
                       ("switch" "switch (${1:variable}) {\n    case ${2:value}: $0break;\n}\n" "switch" nil nil nil "/home/lenz/.config/doom/snippets/cc-mode/switch" nil nil)
                       ("struct" "struct ${1:name} {\n    $0\n};" "struct ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/cc-mode/struct" nil "struct")
                       ("once" "#ifndef ${1:`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H}\n#define $1\n\n$0\n\n#endif /* $1 */" "#ifndef XXX; #define XXX; #endif" nil nil nil "/home/lenz/.config/doom/snippets/cc-mode/once" nil "once")
                       ("math" "#include <math.h>$0" "math" nil nil nil "/home/lenz/.config/doom/snippets/cc-mode/math" nil nil)
                       ("main" "int main(${1:int argc, char *argv[]}) {\n    `%`$0\n    return 0;\n}" "main"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/cc-mode/main" nil nil)
                       ("incc" "#include <$0>" "#include <...>"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/cc-mode/incc" nil nil)
                       ("inc" "#include \"$0\"" "#include \"...\""
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/cc-mode/inc" nil nil)
                       ("ifdef" "#ifdef ${1:MACRO}\n\n$0\n\n#endif // $1" "ifdef"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/cc-mode/ifdef" nil "ifdef")
                       ("if" "if (${1:condition}) {\n    `%`$0\n}" "if (...) { ... }" nil nil nil "/home/lenz/.config/doom/snippets/cc-mode/if" nil "if")
                       ("fori" "for (${1:int} ${2:i} = 0; $2 < ${3:length}; $2++}) {\n    `%`$0\n}" "fori" nil nil nil "/home/lenz/.config/doom/snippets/cc-mode/fori" nil "fori")
                       ("for" "for ($1; $2; $3) {\n    `%`$0\n}" "for" nil nil nil "/home/lenz/.config/doom/snippets/cc-mode/for" nil "for")
                       ("elseif" "else if (${1:condition}) {\n    `%`$0\n}" "elseif" nil nil nil "/home/lenz/.config/doom/snippets/cc-mode/elseif" nil nil)
                       ("else" "else {\n    `%`$0\n}" "else" nil nil nil "/home/lenz/.config/doom/snippets/cc-mode/else" nil nil)
                       ("do" "do {\n    $0\n} while (${1:condition});" "do { ... } while (...)" nil nil nil "/home/lenz/.config/doom/snippets/cc-mode/do" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:47 2024
