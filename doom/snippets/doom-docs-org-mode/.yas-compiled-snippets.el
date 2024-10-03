;;; Compiled snippets and support files for `doom-docs-org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'doom-docs-org-mode
                     '(("<wip" "#+begin_quote\n 🔨 `%`$0\n#+end_quote" "Notice: work-in-progress" nil nil nil "/home/lenz/.config/doom/snippets/doom-docs-org-mode/notice-wip" nil nil)
                       ("<warn" "#+begin_quote\n 🚧 `%`$0\n#+end_quote" "Notice: warning" nil nil nil "/home/lenz/.config/doom/snippets/doom-docs-org-mode/notice-warn" nil nil)
                       ("<tip" "#+begin_quote\n 📌 `%`$0\n#+end_quote" "Notice: user tip" nil nil nil "/home/lenz/.config/doom/snippets/doom-docs-org-mode/notice-tip" nil nil)
                       ("<quote" "#+begin_quote\n 🕞 ${1:`(or % (format-time-string \"%B %d, %Y\"))`}\n#+end_quote" "Notice: timestamp" nil nil nil "/home/lenz/.config/doom/snippets/doom-docs-org-mode/notice-time" nil nil)
                       ("<quote" "#+begin_quote\n 💬 `%`$0\n#+end_quote" "Notice: opinion/tangent" nil nil nil "/home/lenz/.config/doom/snippets/doom-docs-org-mode/notice-quote" nil nil)
                       ("<credit" "#+begin_quote\n 💕 `%`$0\n#+end_quote" "Notice: credit" nil nil nil "/home/lenz/.config/doom/snippets/doom-docs-org-mode/notice-credit" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:47 2024
