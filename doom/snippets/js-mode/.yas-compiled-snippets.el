;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("while" "while (${1:true}) { ${0:`(doom-snippets-format \"%n%s%n\")`} }" "while" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/while" nil nil)
                       ("var" "var ${1:name} = ${0:`%`};" "var ... = ...;" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/var" nil "var")
                       ("try" "try {\n    `%`$0\n} catch (${1:err}) {\n    ${2:// Do something}\n}" "try-catch block" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/try" nil nil)
                       ("r" "return $0;" "return ..." nil nil nil "/home/lenz/.config/doom/snippets/js-mode/return" nil "r")
                       ("require" "require(`%`$0)`(if (eolp) \";\")`" "require(\"...\")" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/require" nil "require")
                       ("req"
                        (progn
                          (doom-snippets-expand :uuid "require"))
                        "require(\"...\")" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/req" nil "req")
                       ("pu" "`(unless (eq (char-before) ?.) \".\")`push(`(doom-snippets-text nil t)`$0)`(if (eolp) \";\")`" "arr.push(elt)" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/push" nil "pu")
                       (":" "${1:key}: ${0:value}" "...: ..." nil nil nil "/home/lenz/.config/doom/snippets/js-mode/property" nil ":")
                       ("m" "${1:method}($2) {\n    `%`$0\n}" "method() { ... }" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/method" nil "m")
                       ("mapfu" "`(unless (eq (char-before) ?.) \".\")`map(function(${1:item}, ${2:i}, ${3:arr}) {\n    `(doom-snippets-format \"%n%s%n\")`$0\n})`(if (eolp) \";\")`" "arr.map(function(item, i, arr) {...})" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/map-function" nil "mapfu")
                       ("map=>" "`(unless (eq (char-before) ?.) \".\")`map((${1:item}, ${2:i}, ${3:arr}) => `(if (> (doom-snippets-count-lines %) 1) (concat \"{ \" (doom-snippets-format \"%n%s%n\") \" }\") %)`$0)`(if (eolp) \";\")`" "arr.map((item, i, arr) => {...})" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/map-arrow" nil "map=>")
                       ("map" "`(unless (eq (char-before) ?.) \".\")`map($0)`(if (eolp) \";\")`" "arr.map(...)" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/map" nil "map")
                       ("log" "console.log(`(doom-snippets-text nil t)`$0)`(if (eolp) \";\")`" "console.log(\"...\");" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/log" nil nil)
                       ("let" "let ${1:name} = ${0:`%`};" "let ... = ...;" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/let" nil nil)
                       ("/**" "/**\n * ${0:`(if % (s-replace \"\\n\" \"\\n * \" %))`}\n */" "/** ... */" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/jsdoc" nil "/**")
                       ("iof" "`(unless (eq (char-before) ?.) \".\")`indexOf(${1:`(or (doom-snippets-text nil t) \"elt\")`}${2: ${3:fromIndex}})`(if (eolp) \";\")`" "arr.indexOf(elt, fromIndex)" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/indexOf" nil "iof")
                       ("import" "import ${1:Object} from '${2:./$3}';" "import ... from ...;" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/import" nil "import")
                       ("imp"
                        (progn
                          (doom-snippets-expand :uuid "import"))
                        "import ... from ...;" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/imp" nil "imp")
                       ("if" "if (${1:true}) {\n    `%`$0\n}" "if" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/if" nil nil)
                       ("gebi" "`(unless (eq (char-before) ?.) \".\")`getElementById(${1:id})`(if (eolp) \";\")`" "getElementById" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/getElementById" nil "gebi")
                       ("function" "function ($1) { `(doom-snippets-format \"%n%s%n\")`$0 }`(if (eolp) \";\")`" "anonymous function"
                        (or
                         (not
                          (doom-snippets-bolp))
                         (region-active-p))
                        nil nil "/home/lenz/.config/doom/snippets/js-mode/function_inline" nil "function_inline")
                       ("function" "function ${1:name}($2) {\n    `(doom-snippets-format \"%n%s%n\")`$0\n}" "named function"
                        (or
                         (doom-snippets-bolp)
                         (region-active-p))
                        nil nil "/home/lenz/.config/doom/snippets/js-mode/function" nil "function_block")
                       ("fu"
                        (progn
                          (doom-snippets-expand :uuid
                                                (if
                                                    (doom-snippets-bolp)
                                                    "function_block" "function_inline")))
                        "anonymous/named function" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/fu" nil nil)
                       ("forin" "for (${1:key} in ${2:list}) {\n    `%`$0\n}" "for (key in list) { ... }" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/forin" nil "forin")
                       ("fori" "for (var ${1:i} = ${2:0}; $1 < ${3:${4:arr}.length}; ++$1) {\n    `%`$0\n}" "for (var i = 0; i < arr.length; ++i) { ... }" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/fori" nil "fori")
                       ("forefu" "`(unless (eq (char-before) ?.) \".\")`forEach(function(${1:item}) {\n    `(doom-snippets-format \"%n%s%n\")`$0\n})`(if (eolp) \";\")`" "arr.forEach(function(item) {...})" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/forEach-function" nil "forefu")
                       ("fore=>" "`(unless (eq (char-before) ?.) \".\")`forEach(${1:item} => `(if (> (doom-snippets-count-lines %) 1) (concat \"{ \" (doom-snippets-format \"%n%s%n\") \" }\") %)`$0)`(if (eolp) \";\")`" "arr.forEach((item) => {...})" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/forEach-arrow" nil "fore=>")
                       ("fore" "`(unless (eq (char-before) ?.) \".\")`forEach(`%`$0)`(if (eolp) \";\")`" "arr.forEach(...)" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/forEach" nil "fore")
                       ("for" "for ($1;$2;$3) {\n    `%`$0\n}" "for (;;)" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/for" nil "for")
                       ("fireEvent" "fireEvent('$0')" "fireEvent" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/fireEvent" nil nil)
                       ("filfu" "`(unless (eq (char-before) ?.) \".\")`filter(function(${1:item}) {\n    `(doom-snippets-format \"%n%s%n\")`$0\n})`(if (eolp) \";\")`" "arr.filter(function(item) {...})" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/filter-function" nil "filfu")
                       ("fil=>" "`(unless (eq (char-before) ?.) \".\")`filter(${1:item} => `(if (> (doom-snippets-count-lines %) 1) (concat \"{ \" (doom-snippets-format \"%n%s%n\") \" }\") %)`$0)`(if (eolp) \";\")`" "arr.filter(item => {...})" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/filter-arrow" nil "fil=>")
                       ("fil" "`(unless (eq (char-before) ?.) \".\")`filter($0)`(if (eolp) \";\")`" "arr.filter(...)" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/filter" nil "fil")
                       ("exp" "module.exports = `%`$0`(if (eolp) \";\")`" "module.exports = ..." nil nil nil "/home/lenz/.config/doom/snippets/js-mode/exports" nil "exports")
                       ("euc" "encodeURIComponent(${1:`%`})`(if (eolp) \";\")`" "encodeURIComponent" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/encodeURIComponent" nil "euc")
                       ("eu" "encodeURI(${1:`%`})`(if (eolp) \";\")`" "encodeURI" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/encodeURI" nil "eu")
                       ("else" "else {\n    `%`$0\n}" "else" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/else" nil nil)
                       ("doc" "document." "document" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/document" nil "doc")
                       ("duc" "decodeURIComponent(${1:`%`})`(if (eolp) \";\")`" "decodeURIComponent" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/decodeURIComponent" nil "duc")
                       ("du" "decodeURI(${1:`%`})`(if (eolp) \";\")`" "decodeURI" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/decodeURI" nil "du")
                       ("constructor" "constructor($1) {\n    `%`$0\n}" "constructor() { ... }" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/constructor" nil nil)
                       ("con" "const ${1:name} = ${0:`%`};" "const ... = ...;" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/const" nil "con")
                       ("class" "class ${1:Name} {\n    $0\n}" "class" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/class" nil nil)
                       ("cl"
                        (progn
                          (doom-snippets-expand :uuid "class"))
                        "class" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/cl" nil "cl")
                       ("=>" "$1 => `(if (> (doom-snippets-count-lines %) 1) (concat \"{ \" (doom-snippets-format \"%n%s%n\") \" }\") %)`$0" "arrow function" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/arrow" nil "=>")
                       ("al" "alert(${0:`%`});" "alert" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/alert" nil "al")
                       ("ael" "`(unless (eq (char-before) ?.) \".\")`addEventListener('${1:DOMContentLoaded}', () => {\n  `%`$0\n})`(if (eolp) \";\")`" "addEventListener" nil nil nil "/home/lenz/.config/doom/snippets/js-mode/addEventListener" nil "ael")
                       ("M" "Math.$0" "Math." nil nil nil "/home/lenz/.config/doom/snippets/js-mode/Math" nil "M")))


;;; Do not edit! File generated at Thu Apr 25 09:20:48 2024
