;;; Compiled snippets and support files for `rjsx-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'rjsx-mode
                     '(("ul" "<ul>\n    <li>`(doom-snippets-format \"%n%s%n\")`$0</li>\n</ul>" "ul > li" nil nil nil "/home/lenz/.config/doom/snippets/rjsx-mode/ul" nil "ul")
                       ("<" "<${1:div}>${0:`(doom-snippets-format \"%n%s%n\")`}</$1>" "HTML/JSX tag" nil nil nil "/home/lenz/.config/doom/snippets/rjsx-mode/tag" nil "<")
                       ("div" "<div>${0:`%`}</div>" "<div></div>" nil nil nil "/home/lenz/.config/doom/snippets/rjsx-mode/div" nil "div")
                       ("</" "<${1:div} `%`$0/>" "HTML/JSX self-closed tag" nil nil nil "/home/lenz/.config/doom/snippets/rjsx-mode/closed-tag" nil "</")))


;;; Do not edit! File generated at Thu Apr 25 09:20:49 2024
