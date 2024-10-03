;;; Compiled snippets and support files for `text-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'text-mode
                     '(("header" "---\ntitle:  ${1:`(replace-regexp-in-string \"[-_.]\" \" \" (file-name-base (or buffer-file-name \"full title\")))`}\nauthor: `user-full-name`\nemail:  `user-mail-address`\ndate:   `(format-time-string \"%F\")`\n---\n" "header" nil nil nil "/home/lenz/.config/doom/snippets/text-mode/header" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:49 2024
