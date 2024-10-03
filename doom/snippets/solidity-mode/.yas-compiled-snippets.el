;;; Compiled snippets and support files for `solidity-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'solidity-mode
                     '(("while" "while (${1:condition}) {\n    `%`$0\n}\n" "while loop" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/while" nil nil)
                       ("struct" "struct ${1:StructName} {\n    $0\n}\n" "struct ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/struct" nil nil)
                       ("require" "require($0);\n" "require(...);" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/require" nil nil)
                       ("modifier" "modifier ${1:modifierName}(${2:args}) {\n    $0\n    ${3:_;}\n}\n" "modifier ...(...) { ... }" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/modifier" nil nil)
                       ("library" "library ${1:LibraryName} {\n    $0\n}\n" "library ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/library" nil nil)
                       ("interface" "interface ${1:InterfaceName} {\n    $0\n}\n" "interface ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/interface" nil nil)
                       ("ife" "if (${1:condition}) {\n    `%`$2\n} else {\n    $0\n}" "if (...) { ... } else { ... }" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/ife" nil nil)
                       ("if" "if (${1:condition}) {\n    `%`$0\n}\n" "if (...) { ... }" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/if" nil nil)
                       ("function" "function ${1:functionName}(${2:args})${3: internal}${4: returns (${5:return types})} {\n    $0\n}\n" "function ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/function" nil nil)
                       ("for" "for (${1:uint} ${2:i} = 0; $2 < ${3:${4:array}.length}; $2++) {\n    `%`$0\n}\n" "for loop" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/for" nil nil)
                       ("event" "event ${1:EventName}(${2:args});\n" "event ...(...)" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/event" nil nil)
                       ("enum" "enum ${1:EnumName} {\n    $0\n}\n" "enum ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/enum" nil nil)
                       ("contract" "contract ${1:ContractName} {\n    $2\n\n    constructor(${3:args}) public {\n        $0\n    }\n}\n" "contract ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/contract" nil nil)
                       ("assert" "assert($0);" "assert(...);" nil nil nil "/home/lenz/.config/doom/snippets/solidity-mode/assert" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:49 2024
