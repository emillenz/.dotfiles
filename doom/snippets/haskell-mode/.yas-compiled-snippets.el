;;; Compiled snippets and support files for `haskell-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'haskell-mode
                     '(("pr" "print $0" "print" nil nil nil "/home/lenz/.config/doom/snippets/haskell-mode/print" nil "pr")
                       ("{" "{-# ${1:PRAGMA} #-}" "pragma" nil nil nil "/home/lenz/.config/doom/snippets/haskell-mode/pragma" nil "{")
                       ("class" "class ${1:Class Name} where\n      $0" "new class" nil nil nil "/home/lenz/.config/doom/snippets/haskell-mode/new class" nil "class")
                       ("mod" "module ${1:Module} where\n$0" "module" nil nil nil "/home/lenz/.config/doom/snippets/haskell-mode/module" nil "mod")
                       ("main" "main = do $0" "main" nil nil nil "/home/lenz/.config/doom/snippets/haskell-mode/main" nil "main")
                       ("ins" "instance ${1:${2:(Show a)} => }${3:Ord} ${4:DataType} where\n$0\n" "instance" nil nil nil "/home/lenz/.config/doom/snippets/haskell-mode/instance" nil "ins")
                       ("import" "import${1: qualified} ${2:Module${3:(symbols)}}${4: as ${5:alias}}" "import" nil nil nil "/home/lenz/.config/doom/snippets/haskell-mode/import" nil "import")
                       ("::" "${1:fn-name} :: ${2:type}\n$1" "Function" nil nil nil "/home/lenz/.config/doom/snippets/haskell-mode/function" nil "::")
                       ("d" "{-\n  $0\n-}" "doc" nil nil nil "/home/lenz/.config/doom/snippets/haskell-mode/doc" nil "d")
                       ("da" "data ${1:Type} = $2" "data" nil nil nil "/home/lenz/.config/doom/snippets/haskell-mode/data" nil "da")
                       ("case" "case ${1:var} of\n     ${2:cond} -> ${3:value}\n     $0\n     otherwise -> ${4:other}" "case" nil nil nil "/home/lenz/.config/doom/snippets/haskell-mode/case" nil "case")))


;;; Do not edit! File generated at Thu Apr 25 09:20:48 2024
