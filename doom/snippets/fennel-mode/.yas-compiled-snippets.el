;;; Compiled snippets and support files for `fennel-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'fennel-mode
                     '(("while" "(while ${1:t}`(if % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)" "while" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/while" nil nil)
                       ("when" "(when ${1:t}`(if % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)" "when" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/when" nil nil)
                       ("var" "(var ${1:varname} ${2:value})" "var" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/var" nil nil)
                       ("tset" "(tset ${1:table} ${2:field} ${3:value})" "tset" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/tset" nil nil)
                       ("tc" "(table.concat ${1:table} ${2:sep}${3: ${4:start} ${5:end}})" "table.concat" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/table.concat" nil nil)
                       ("set" "(set ${1:varname} ${2:value})" "set" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/set" nil nil)
                       ("req" "(require ${1::module})" "require" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/require" nil nil)
                       ("p" "(print ${1:message})" "print" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/print" nil nil)
                       ("local" "(local ${1:varname} ${2:value})" "local" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/local" nil nil)
                       ("let" "(let [$1] `(doom-snippets-format \"%n%s\")`)`(doom-snippets-newline-or-eol)`" "let" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/let" nil nil)
                       ("len" "(length ${1:var})" "length" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/length" nil nil)
                       ("lam" "(lambda [$1]`(and % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)`(doom-snippets-newline-or-eol)`" "lambda" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/lambda" nil nil)
                       ("if" "(if ${1:t}`(if % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)" "if" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/if" nil nil)
                       ("global" "(global ${1:varname} ${2:value})" "global" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/global" nil nil)
                       ("for" "(for [${1:i} ${2:1} ${3:10}]`(if % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)" "for" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/for" nil nil)
                       ("fn" "(fn ${1:name} [$2]`(and % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)`(doom-snippets-newline-or-eol)`" "fn" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/fn" nil nil)
                       ("each" "(each [${1:key} ${2:value} ${3:list}]`(if % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)" "each" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/each" nil nil)
                       ("do" "(do`(if % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)" "do" nil nil nil "/home/lenz/.config/doom/snippets/fennel-mode/do" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:47 2024
