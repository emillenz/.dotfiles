;;; Compiled snippets and support files for `+javascript-lb6-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets '+javascript-lb6-mode
                     '(("preferences" "Action.preferences.${1:name}" "Action.preferences.?" nil nil nil "/home/lenz/.config/doom/snippets/+javascript-lb6-mode/preferences" nil nil)
                       ("lbupdate" "<key>LBChangelog</key>\n<string>$1</string>\n\n<key>LBUpdateURL</key>\n<string>https://raw.githubusercontent.com/hlissner/lb6-${2:actions}/master/${3:Name of Action}.lbaction/Contents/Info.plist</string>\n\n<key>LBDownloadURL</key>\n<string>https://dl.v0.io/launchbar/$3.lbaction</string>\n\n" "lbupdate"
                        (eq major-mode 'nxml-mode)
                        nil nil "/home/lenz/.config/doom/snippets/+javascript-lb6-mode/lbupdate" nil "lbupdate")
                       ("include" "include(\"${1:shared/lib/${2:lib.js}}\");" "include(...)" nil nil nil "/home/lenz/.config/doom/snippets/+javascript-lb6-mode/include" nil nil)
                       ("LBSuggestionsScript" "<key>LBSuggestionsScript</key>\n<dict>\n    <key>LBScriptName</key>\n    <string>${1:suggestions}.js</string>\n\n    <key>LBBackgroundKillEnabled</key>\n    <true />\n</dict>" "LBSuggestionsScript" nil nil nil "/home/lenz/.config/doom/snippets/+javascript-lb6-mode/LBSuggestionsScript" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:46 2024
