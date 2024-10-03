;;; Compiled snippets and support files for `kotlin-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'kotlin-mode
                     '(("while" "while ($true) {\n    $0\n}\n" "while" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/while" nil nil)
                       ("var=" "var ${1:variable}${2:: ${3:Int}} = `%`$0\n" "var=" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/var=" nil nil)
                       ("var" "var ${1:variable}: ${2:Int}\n" "var" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/var" nil nil)
                       ("val=" "val ${1:name}${2:: ${3:Int}} = `%`$0" "val=" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/val=" nil nil)
                       ("val" "val ${1:name}: ${2:Int}" "val" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/val" nil nil)
                       ("todo" "TODO('Not yet implemented')\n" "todo" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/todo" nil nil)
                       ("main" "fun main(args: Array<String>) {\n    $0\n}\n" "main" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/main" nil nil)
                       ("interface" "interface ${1:name} {\n    $0\n}\n" "interface" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/interface" nil nil)
                       ("ife" "if (${1:true}) {\n    `%`$2\n} else {\n    $0\n}\n" "ife" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/ife" nil nil)
                       ("if" "if (${1:true}) {\n    $0\n}\n" "if" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/if" nil nil)
                       ("fun" "fun ${1:name}($2)${3:: ${4:Unit}} {\n    ${0:TODO('Not yet implemented')}\n}\n" "fun" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/fun" nil nil)
                       ("forin" "for (${1:key} in ${2:iterable}) {\n    `%`$0\n}\n" "for (key in iterable) { ... }" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/forin" nil nil)
                       ("file_class" "class `(f-base buffer-file-name)`${1:($2)}${3: {\n    $0\n}}\n" "file_class" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/file_class" nil nil)
                       ("class" "class ${1:name}${2:($3)}${4: : $5}${6: {\n    $0\n}}\n" "class" nil nil nil "/home/lenz/.config/doom/snippets/kotlin-mode/class" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:48 2024
