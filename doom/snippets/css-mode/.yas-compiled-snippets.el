;;; Compiled snippets and support files for `css-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'css-mode
                     '((":" "${1:prop}: ${2:`%`};" "...: ...;" nil nil nil "/home/lenz/.config/doom/snippets/css-mode/property" nil ":")
                       ("pad" "padding: ${1:10px};" "padding: ...;" nil nil nil "/home/lenz/.config/doom/snippets/css-mode/padding" nil "pad")
                       ("media_print" "@media print {\n    `%`$0\n}" "@media print { ... }" nil nil nil "/home/lenz/.config/doom/snippets/css-mode/media_print" nil nil)
                       ("media_orientation" "(orientation: ${1:landscape})" "@media (orientation: ?)"
                        (looking-back "@media "
                                      (line-beginning-position))
                        nil nil "/home/lenz/.config/doom/snippets/css-mode/media_orientation" nil nil)
                       ("med" "@media ${1:screen} {\n    `%`$0\n}" "@media" nil nil nil "/home/lenz/.config/doom/snippets/css-mode/media" nil "med")
                       ("mar" "margin: ${1:0 auto};" "margin: ...;" nil nil nil "/home/lenz/.config/doom/snippets/css-mode/margin" nil "mar")
                       ("impurl" "@import url(\"`(doom-snippets-text nil t)`$0\");" "@import url(...)" nil nil nil "/home/lenz/.config/doom/snippets/css-mode/importurl" nil "impurl")
                       ("impfont" "@import url(\"http://fonts.googleapis.com/css?family=${1:Open+Sans}\");" "@import url(\"//fonts.googleapis...\")" nil nil nil "/home/lenz/.config/doom/snippets/css-mode/importfont" nil "impfont")
                       ("imp" "@import \"`(doom-snippets-text nil t)`$0\";" "@import" nil nil nil "/home/lenz/.config/doom/snippets/css-mode/import" nil "imp")))


;;; Do not edit! File generated at Thu Apr 25 09:20:47 2024
