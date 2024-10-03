;;; Compiled snippets and support files for `buttercup-minor-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'buttercup-minor-mode
                     '(("scc" "(spy-calls-count ${1:'foo})" "spy-calls-count ..." nil nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/spy-calls-count" nil "scc")
                       ("scaf" "(spy-calls-args-for ${1:'foo} ${2:args...})" "spy-calls-args-for ..." nil nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/spy-calls-args-for" nil "scaf")
                       ("sca" "(spy-calls-any ${1:'foo})" "spy-calls-any ..." nil nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/spy-calls-any" nil "sca")
                       ("scaa" "(spy-calls-all-args ${1:'foo})" "spy-calls-all-args ..." nil nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/spy-calls-all-args" nil "scaa")
                       ("ate" "and-throw-error ${1:'error}" ":and-throw-error ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/spy-and-throw-error" nil "ate")
                       ("arv" "and-return-value ${1:value}" ":and-return-value ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/spy-and-return-value" nil "arv")
                       ("act" "and-call-through" ":and-call-through ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/spy-and-call-through" nil "act")
                       ("acf" "and-call-fake ${1:#'fn}" ":and-call-fake ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/spy-and-call-fake" nil "acf")
                       ("it" "(it \"${1:...}\" ${2:`(doom-snippets-format \"%n%s\")`}$0)" "it ..." nil nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/it" nil nil)
                       ("t" "to-throw" ":to-throw"
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/expect-to-throw" nil "t")
                       ("m" "to-match $1" ":to-match ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/expect-to-match" nil "m")
                       ("s" "to-have-same-items-as $1" ":to-have-same-items-as ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/expect-to-have-same-items-as" nil "s")
                       ("bcw" "to-have-been-called-with ${1:args...}" ":to-have-been-called-with ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/expect-to-have-been-called-with" nil "bcw")
                       ("bct" "to-have-been-called-times ${1:1}" ":to-have-been-called-time ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/expect-to-have-been-called-times" nil "bct")
                       ("bc" "to-have-been-called" ":to-have-been-called ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/expect-to-have-been-called" nil "bc")
                       ("e" "to-equal $1" ":to-equal"
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/expect-to-equal" nil "e")
                       ("c" "to-contain $1" ":to-contain ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/expect-to-contain" nil "c")
                       ("b" "to-be ${1:nil}" ":to-be ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/expect-to-be" nil "b")
                       ("expect" "(expect ${1:`(doom-snippets-format \"value\")`}$0)" "expect" nil nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/expect" nil "expect")
                       ("ex"
                        (progn
                          (doom-snippets-expand :uuid "expect"))
                        "expect" nil nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/ex" nil "ex")
                       ("describe" "(describe \"${1:group}\"${2:`(doom-snippets-format \"%n%s\")`}$0)" "describe ... ..." nil nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/describe" nil "describe")
                       ("desc"
                        (progn
                          (doom-snippets-expand :uuid "describe"))
                        "describe ... ..." nil nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/desc" nil "desc")
                       ("bfe" "(before-each ${1:`(doom-snippets-format \"%n%s\")`})" "before-each ..." nil nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/before-each" nil "bfe")
                       ("bfa" "(before-all ${1:`(doom-snippets-format \"%n%s\")`})" "before-all ..." nil nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/before-all" nil "bfa")
                       ("afe" "(after-each ${1:`(doom-snippets-format \"%n%s\")`})" "after-each ..." nil nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/after-each" nil "afe")
                       ("afa" "(after-all ${1:`(doom-snippets-format \"%n%s\")`})" "after-all ..." nil nil nil "/home/lenz/.config/doom/snippets/buttercup-minor-mode/after-all" nil "afa")))


;;; Do not edit! File generated at Thu Apr 25 09:20:46 2024
