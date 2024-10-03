;;; Compiled snippets and support files for `typescript-tsx-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'typescript-tsx-mode
                     '(("fct" "import React from 'react';\n\ninterface ${1:`(file-name-base buffer-file-name)`}PropTypes {\n\n}\n\nconst $1 = (props: $1PropTypes) => (\n  $0\n);\n\nexport { $1 };\n" "functionalComponentTSX" nil nil nil "/home/lenz/.config/doom/snippets/typescript-tsx-mode/componentTSX" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:49 2024
