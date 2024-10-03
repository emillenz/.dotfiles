;; [[file:config.org::*user][user:1]]
(setq user-full-name "emil lenz"
      user-mail-address "emillenz@protonmail.com")
;; user:1 ends here

;; [[file:config.org::*modus-theme][modus-theme:1]]
(use-package! modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-common-palette-overrides `((fg-region unspecified) ;; NOTE :: don't override syntax highlighting in region
                                                (fg-heading-1 fg-heading-0)
                                                (bg-prose-block-contents bg-dim)))

  (custom-set-faces!
    `(org-list-dt :inherit modus-themes-heading-1)
    `(org-block-begin-line :foreground ,(modus-themes-get-color-value 'prose-metadata))
    `(org-quote :slant italic))

  (setq doom-theme 'modus-operandi))
;; modus-theme:1 ends here

;; [[file:config.org::*font][font:1]]
(setq doom-font                (font-spec :family "Iosevka Comfy" :size 13)
      doom-variable-pitch-font (font-spec :family "Iosevka Comfy" :size 13)
      doom-serif-font          (font-spec :family "Iosevka Comfy" :size 13)
      doom-big-font            (font-spec :family "Iosevka Comfy" :size 28))
;; font:1 ends here

;; [[file:config.org::*modeline][modeline:1]]
(setq display-battery-mode nil
      display-time-mode nil
      +modeline-height 8
      +modeline-bar-width nil) ;; visual clutter => off
;; modeline:1 ends here

;; [[file:config.org::*window layout & behavior][window layout & behavior:1]]
(setq evil-vsplit-window-right t
      evil-split-window-below t
      even-window-sizes 'width-only
      window-combination-resize t
      split-height-threshold nil
      split-width-threshold 0)

(after! org
  (setq org-src-window-setup 'current-window
        org-agenda-window-setup 'current-window))

(add-to-list 'display-buffer-alist
             `("^\\.*" display-buffer-reuse-mode-window))
;; window layout & behavior:1 ends here

;; [[file:config.org::*window layout & behavior][window layout & behavior:2]]
;; HACK :: cannot use 'global-visual-fill-column-mode' (doesn't work in many buffers).  do NOT enable for 'prog-mode' (breaks with flycheck display)
(add-hook! '(text-mode-hook
             dired-mode-hook
             conf-mode-hook
             Info-mode-hook
             org-agenda-mode-hook
             magit-mode-hook)
           #'visual-fill-column-mode)
(global-display-fill-column-indicator-mode 0)

(setq-default visual-fill-column-enable-sensible-window-split t
              visual-fill-column-center-text t
              visual-fill-column-width 100
              fill-column 100)
;; window layout & behavior:2 ends here

;; [[file:config.org::*misc options][misc options:1]]
(setq initial-scratch-message ""
      delete-by-moving-to-trash t
      bookmark-default-file "~/.config/doom/bookmarks" ;; save bookmarks in config dir (preserve for newinstalls)
      auto-save-default t
      confirm-kill-emacs nil
      hscroll-margin 0
      scroll-margin 0
      enable-recursive-minibuffers nil
      display-line-numbers-type 'visual
      shell-command-prompt-show-cwd t
      async-shell-command-width 100
      shell-file-name "/usr/bin/fish")

(save-place-mode 1)
(+global-word-wrap-mode 1)
(global-subword-mode 1)
(add-hook! '(prog-mode-hook conf-mode-hook) #'rainbow-delimiters-mode)
;; misc options:1 ends here

;; [[file:config.org::*leader (\[\[kbd:SPC\]\[SPC\]\], \[\[kbd:,\]\[,\]\])][leader ([[kbd:SPC][SPC]], [[kbd:,][,]]):1]]
(setq doom-leader-key "SPC"
      doom-leader-alt-key "C-SPC"
      doom-localleader-key ","
      doom-localleader-alt-key "C-,")

(map! :leader
      "." #'vertico-repeat
      "'" #'consult-bookmark
      (:prefix "s"
               "K" #'devdocs-lookup
               "t" #'dictionary-search
               "g" #'occur)
      (:prefix "f"
               "F" #'+vertico/consult-fd-or-find) ;; HACK :: fix original binding
      (:prefix "c"
               "r" #'lsp-rename
               (:prefix "'"
                        "t" #'org-babel-tangle
                        "T" #'org-babel-detangle))
      (:prefix "n"
               "g" #'org-capture-goto-last-stored)
      (:prefix "t"
               "c" #'global-visual-fill-column-mode))
;; leader ([[kbd:SPC][SPC]], [[kbd:,][,]]):1 ends here

;; [[file:config.org::*global navigation scheme][global navigation scheme:1]]
(map! :map 'override
      :nm "C-w" #'next-window-any-frame
      :nm "C-q" #'kill-buffer-and-window
      :nm "C-s" #'basic-save-buffer  ;; statistically most called command => ergonomic (& default) mapping
      :i  "C-v" #'evil-paste-after ;; more convenient that hitting C-r"
      :nm "C-e" #'find-file
      :nm "C-f" #'projectile-find-file
      :nm "C-b" #'consult-buffer
      :nm "C-<tab>" #'evil-switch-to-windows-last-buffer ;; consistent with browser/WM
      :nm "<tab>" #'harpoon-quick-menu-hydra) ;; without modifier, in order to allow for fast tab-<num> combos.  additionally consistent with tmux.  NOTE:: org mode makes heavy use of folding, and we should make use of vim's folding bindings using z-prefix to get more fine grained control instead of using <tab>.

(defadvice! z-goto-global-mark (char)
  "Go to the buffer of the global-mark.
Usage: 'evil-set-mark' <uppercase> 'goto-global-mark' <lowercase>.  (faster/more ergonomic)"
  :override #'evil-goto-mark-line
  (let (marker (evil-get-marker (upcase char)))
    (unless (numberp marker)
      (switch-to-buffer (marker-buffer marker)))))
;; global navigation scheme:1 ends here

;; [[file:config.org::*vim editing][vim editing:1]]
(map! :after evil
      :n    "C-j" #'newline-and-indent  ;; useful inverse of 'J'
      :nmv  "&"   #'evil-ex-repeat ;; more extensible than normal '&'
      :nmv  "("   #'backward-sexp  ;; more useful than navigation by sentences
      :nmv  ")"   #'forward-sexp
      :nmv  "+"   #'evil-numbers/inc-at-pt ;; more sensible than C-x/C-a
      :nmv  "-"   #'evil-numbers/dec-at-pt
      :nmv  "g+"  #'evil-numbers/inc-at-pt-incremental
      :nmv  "g-"  #'evil-numbers/dec-at-pt-incremental ;; more powerful '/' => preview matches interactively (better than vim's: C-g/C-t in search-mode)
      :nmv  "g<"  #'evil-lion-left
      :nmv  "g>"  #'evil-lion-right
      :nmv  "s"   #'evil-surround-region ;; vim's <s/S> is useless (same as <x> and <C>)
      :nmv  "S"   #'evil-Surround-region)

;; HACK :: needed to make 'C-h' work as backspace consistently, everywhere (some modes override it to <help>).
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
;; vim editing:1 ends here

;; [[file:config.org::*org_][org_:1]]
(map! :localleader :map org-mode-map :after org
      "\\" #'org-latex-preview
      ","  #'org-ctrl-c-ctrl-c
      "-"  #'org-toggle-item
      "["  (cmd! (let ((current-prefix-arg '(4)))
                   (call-interactively #'org-toggle-checkbox)))
      "z"  #'org-add-note)
;; org_:1 ends here

;; [[file:config.org::*dired_][dired_:1]]
(map! :map dired-mode-map :after dired
      :nm "h" #'dired-up-directory
      :nm "l" #'dired-open-file
      :nm "." #'dired-omit-mode)
      ;; create new files using `find-file' (inserts filetemplate properly)

(map! :after dired :map dired-mode-map :localleader
      :nm "a" #'z-dired-archive)
;; dired_:1 ends here

;; [[file:config.org::*editor][editor:1]]
(evil-surround-mode 1)
(after! evil
  (setq evil-want-fine-undo nil
        evil-ex-substitute-global t
        evil-want-C-i-jump t
        evil-want-C-h-delete t
        evil-want-minibuffer t ;; don't loose all your power's in the minibuffer
        evil-org-use-additional-insert nil))

(defadvice! z-update-evil-search-reg ()
  "Update evil search register after jumping to a line with
`+default/search-buffer' to be able to jump to next/prev matches.
This is sensible default behaviour, and integrates it into evil."
  :after #'+default/search-buffer
  (let ((str (string-replace
              " " ".*"
              (car consult--line-history))))
    (push str evil-ex-search-history)
    (setq evil-ex-search-pattern (list str t t))))
;; editor:1 ends here

;; [[file:config.org::*jumplist][jumplist:1]]
(dolist (cmd '(flycheck-next-error
               flycheck-previous-error
               +lookup/definition
               +lookup/references
               +lookup/implementations
               +default/search-buffer
               consult-imenu))
  (evil-add-command-properties cmd :jump t))

(dolist (cmd '(evil-backward-section-begin
               evil-forward-section-begin
               evil-jump-item
               evil-backward-paragraph
               evil-forward-paragraph
               evil-forward-section-end))
  (evil-remove-command-properties cmd :jump))
;; jumplist:1 ends here

;; [[file:config.org::*completion][completion:1]]
(vertico-flat-mode 1)

(after! company
  (setq company-minimum-prefix-length 0
        consult-async-min-input 0 ;; immediate
        company-idle-delay nil ;; manually trigger
        company-tooltip-idle-delay 0.1 ;; faster
        company-show-quick-access t
        company-global-modes '(not
                               help-mode
                               eshell-mode
                               org-mode
                               vterm-mode)))

(map! :after minibuffer :map minibuffer-local-map ;; consistency with vim bindings
      :i "C-n" #'next-line-or-history-element
      :i "C-p" #'previous-line-or-history-element)

(map! :after company :map company-mode-map
      :i "C-n" #'company-complete)

(map! :map vertico-map
      :im "C-w" #'vertico-directory-delete-word ;; smarter C-w
      :im "C-d" #'consult-dir
      :im "C-f" #'consult-dir-jump-file)
;; completion:1 ends here

;; [[file:config.org::*snippets][snippets:1]]
(setq yas-triggers-in-field t)
;; snippets:1 ends here

;; [[file:config.org::*file templates][file templates:1]]
(set-file-templates!
 '(org-mode :trigger "header")
 '(prog-mode :trigger "header"))
;; file templates:1 ends here

;; [[file:config.org::*dired][dired:1]]
(after! dired
  (add-hook! 'dired-mode-hook #'dired-hide-details-mode) ;; less clutter (enable manually if needed)
  (setq dired-open-extensions (mapcan (lambda (pair)
                                        (mapcar (lambda (ext)
                                                  (cons ext (cdr pair)))
                                                (car pair)))
                                      '((("mkv" "webm" "mp4" "mp3") . "mpv")
                                        (("gif" "jpeg" "jpg" "png") . "nsxiv")
                                        (("docx" "odt" "odf")       . "libreoffice")
                                        (("epub" "pdf")             . "zathura")))
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        global-auto-revert-non-file-buffers t
        dired-no-confirm '(uncompress move copy)
        dired-omit-files "^\\..*$"))
;; dired:1 ends here

;; [[file:config.org::*archive file][archive file:1]]
(defvar z-archive-dir "~/Archive/")

(defun z-dired-archive ()
  "`mv' marked file/s to: `z-archive-dir'/{relative-filepath-to-HOME}/{filename}"
  (interactive)
  (mapc (lambda (file)
          (let* ((dest (file-name-concat z-archive-dir
                                         (file-relative-name file "~/")))
                 (dir (file-name-directory dest)))
            (unless (file-exists-p dir)
              (make-directory dir t))
            (rename-file file dest 1)))
        (dired-get-marked-files nil nil))
  (revert-buffer))
;; archive file:1 ends here

;; [[file:config.org::*indentation][indentation:1]]
(advice-add #'doom-highlight-non-default-indentation-h :override #'ignore)

(defvar z-indent-width 8)

(setq-default standard-indent z-indent-width
              evil-shift-width z-indent-width
              tab-width z-indent-width
              fill-column 100
              tab-always-indent t
              tab-width z-indent-width
              org-indent-indentation-per-level z-indent-width
              evil-indent-convert-tabs t
              indent-tabs-mode nil)

(setq-hook! '(c-mode-hook java-mode-hook)
  c-basic-offset z-indent-width)

(setq-hook! 'ruby-mode-hook
  evil-shift-width z-indent-width
  ruby-indent-level z-indent-width)

(setq-hook! 'rustic-mode-hook
  rustic-indent z-indent-width
  rustic-indent-offset z-indent-width)
;; indentation:1 ends here

;; [[file:config.org::*begin org][begin org:1]]
(after! org
;; begin org:1 ends here

;; [[file:config.org::*options][options:1]]
(add-hook! 'org-mode-hook '(visual-line-mode
                            org-fragtog-mode
                            rainbow-mode
                            laas-mode
                            +org-pretty-mode
                            org-appear-mode))
(setq-hook! 'org-mode-hook
  warning-minimum-level :error) ;; prevent frequent popups of *warning* buffer

(setq org-use-property-inheritance t
      org-reverse-note-order t
      org-startup-with-latex-preview t
      org-startup-with-inline-images t
      org-startup-indented t
      org-startup-numerated t
      org-startup-align-all-tables t
      org-list-allow-alphabetical t
      org-tags-column 0
      org-fold-catch-invisible-edits 'smart
      org-refile-use-outline-path 'full-file-path
      org-refile-allow-creating-parent-nodes 'confirm
      org-use-sub-superscripts '{}
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-block-delimiter-line t
      doom-themes-org-fontify-special-tags t
      org-ellipsis "…"
      org-num-max-level 3
      org-hide-leading-stars t
      org-appear-autoemphasis t
      org-appear-autosubmarkers t
      org-appear-autolinks t
      org-appear-autoentities t
      org-appear-autokeywords t
      org-appear-inside-latex nil
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-pretty-entities-include-sub-superscripts t
      org-list-demote-modify-bullet '(("-"  . "-")
                                      ("+"  . "+")
                                      ("*"  . "-")
                                      ("a." . "a)")
                                      ("1." . "1)")
                                      ("1)" . "a)"))
      org-blank-before-new-entry '((heading . t)
                                   (plain-list-item . nil))
      org-src-ask-before-returning-to-edit-buffer nil)
;; options:1 ends here

;; [[file:config.org::*symbols][symbols:1]]
(add-hook! 'org-mode-hook '(org-superstar-mode
                            prettify-symbols-mode))

(setq org-superstar-headline-bullets-list '("◉" "◯" "▣" "□" "◈" "◇"))

(setq org-superstar-item-bullet-alist '((?- . "─")
                                        (?* . "─") ;; NOTE :: asteriks are reserved for headings only (don't use in lists) => no unambigiuity
                                        (?+ . "⇒")))

(appendq! +ligatures-extra-symbols '(:em_dash       "—"
                                     :ellipses      "…"
                                     :arrow_right   "→"
                                     :arrow_left    "←"
                                     :arrow_lr      "↔"))

(add-hook! 'org-mode-hook
  (appendq! prettify-symbols-alist '(("--"  . "–")
                                     ("---" . "—")
                                     ("->" . "→")
                                     ("=>" . "⇒")
                                     ("<=>" . "⇔"))))
;; symbols:1 ends here

;; [[file:config.org::*task states][task states:1]]
(setq org-todo-keywords '((sequence
                           "[ ](t)"
                           "[@](e)"
                           "[?](?!)"
                           "[-](-!)"
                           "[>](>!)"
                           "[=](=!)"
                           "[&](&!)"
                           "|"
                           "[x](x!)"
                           "[\\](\\!)")))

(setq org-todo-keyword-faces '(("[@]"  . (bold +org-todo-project))
                               ("[ ]"  . (bold org-todo))
                               ("[-]"  . (bold +org-todo-active))
                               ("[>]"  . (bold +org-todo-onhold))
                               ("[?]"  . (bold +org-todo-onhold))
                               ("[=]"  . (bold +org-todo-onhold))
                               ("[&]"  . (bold +org-todo-onhold))
                               ("[\\]" . (bold org-done))
                               ("[x]"  . (bold org-done))))
;; task states:1 ends here

;; [[file:config.org::*task states][task states:2]]
(setq org-log-done 'time
      org-log-repeat 'time
      org-todo-repeat-to-state "[ ]"
      org-log-redeadline 'time
      org-log-reschedule 'time
      org-log-into-drawer "LOG")

(setq org-priority-highest 1
      org-priority-lowest 3)

(setq org-log-note-headings '((done        . "note-done: %t")
                              (state       . "state: %-3S -> %-3s %t") ;; NOTE :: the custom task-statuses are all 3- wide
                              (note        . "note: %t")
                              (reschedule  . "reschedule: %S, %t")
                              (delschedule . "noschedule: %S, %t")
                              (redeadline  . "deadline: %S, %t")
                              (deldeadline . "nodeadline: %S, %t")
                              (refile      . "refile: %t")
                              (clock-out   . "")))
;; task states:2 ends here

;; [[file:config.org::*babel][babel:1]]
(setq org-babel-default-header-args '((:session  . "none")
                                      (:results  . "replace")
                                      (:exports  . "code")
                                      (:cache    . "no")
                                      (:noweb    . "no")
                                      (:hlines   . "no")
                                      (:tangle   . "no")
                                      (:mkdirp   . "yes")
                                      (:comments . "link")))
;; babel:1 ends here

;; [[file:config.org::*clock][clock:1]]
(setq org-clock-out-when-done t
      org-clock-persist t
      org-clock-into-drawer t)
;; clock:1 ends here

;; [[file:config.org::*capture templates][capture templates:1]]
(setq org-directory "~/Documents/org/")
(defvar z-org-journal-dir (file-name-concat "~/Documents/journal/")
  "captured daily journal files")
(defvar z-org-literature-dir "~/Documents/literature"
  "literature sources and captured notes")
(defvar z-org-literature-notes-dir (file-name-concat z-org-literature-dir "notes/")
  "note files for each literature source")
(defvar z-wiki-dir "~/Documents/wiki/"
  "personal knowledge base directory :: cohesive, structured, standalone articles/guides.
(blueprints and additions to these articles are captured into 'org-directory/personal/notes.org',
and the later reviewed and merged into the corresponding article of the wiki.")

(defun z-doct-journal-file (&optional time)
  "TIME :: time in day of note to return. (default: today)"
  (file-name-concat z-org-journal-dir
                    (format "%s_journal.org"
                            (format-time-string "%F" (or time (current-time))))))

(defvar z-doct-projects '(("cs" :keys "c"
                           :children (("ti"   :keys "t")
                                      ("an2"  :keys "a")
                                      ("spca" :keys "s")
                                      ("ph1"  :keys "p")
                                      ("nm"   :keys "n")))
                          ("personal" :keys "p")
                          ("config"   :keys "f")))

(defun z-doct-projects-file (type path)
  "TYPE :: 'agenda | 'notes"
  (file-name-concat org-directory
                    path
                    (format "%s.org" (symbol-name type))))

(defun z-doct-task-template (path)
  (list "task"
        :keys "t"
        :file (z-doct-projects-file 'agenda path)
        :headline "inbox"
        :prepend t
        :empty-lines-after 1
        :template '("* [ ] %^{title}%?")))

(defun z-doct-event-template (path)
  (list "event"
        :keys "e"
        :file (z-doct-projects-file 'agenda path)
        :headline "events"
        :prepend t
        :empty-lines-after 1
        :template '("* [@] %^{title}%?"
                    "%^T"
                    ":PROPERTIES:"
                    ":REPEAT_TO_STATE: [@]" ; NOTE :: in case is made repeating
                    ":location: %^{location}"
                    ":material: %^{material}"
                    ":END:")))

(defun z-doct-note-template (path)
  (list "note"
        :keys "n"
        :file (z-doct-projects-file 'notes path)
        :prepend t
        :empty-lines 1
        :template '("* %^{title} %^g"
                    ":PROPERTIES:"
                    ":created: %U"
                    ":END:"
                    "%?")))

(defun z-doct-expand-templates (projects &optional parent-path)
  "PROJECTS :: `z-doct-projects'
PARENT-PATH :: nil (used for recursion)"
  (mapcar (lambda (project)
            (let* ((tag (car project))
                   (props (cdr project))
                   (key (plist-get props :keys))
                   (self `(,tag :keys ,key))
                   (children (plist-get props :children))
                   (path (file-name-concat parent-path tag)))
              (append self
                      (if children
                          (list :children (append (z-doct-expand-templates children path)
                                                  (z-doct-expand-templates (list self) nil))) ;; NOTE :: don't nest self in it's own subdir
                        (list :children (list (z-doct-task-template path)
                                              (z-doct-event-template path)
                                              (z-doct-note-template path)))))))
          projects))

(setq org-capture-templates
      (doct `(,@(z-doct-expand-templates z-doct-projects)

              ("journal"
               :keys "j"
               :file (lambda () (z-doct-journal-file))
               :title (lambda () (downcase (format-time-string "journal: %A, %e. %B %Y")))
               :children (("journal init"
                           :keys "j"
                           :type plain
                           :template  ("#+title:  %{title}"
                                       "#+author: %(user-full-name)"
                                       "#+email:  %(message-user-mail-address)"
                                       "#+date:   %<%F>"
                                       "#+filetags: :journal:"
                                       ""
                                       "* goals"
                                       "- [ ] %?"
                                       ""
                                       "* agenda"
                                       "** [ ] "))

                          ("note"
                           :keys "n"
                           :headline "notes"
                           :prepend t
                           :empty-lines-after 1
                           :template ("* %^{title}"
                                      ":PROPERTIES:"
                                      ":created: %U"
                                      ":END:"
                                      "%?"))

                          ("yesterday review"
                           :keys "y"
                           :unnarrowed t
                           :file (lambda ()
                                   (z-doct-journal-file (time-subtract (current-time) (days-to-time 1))))
                           :template ("* gratitude"
                                      "- %?"
                                      ""
                                      "* reflection"
                                      "-"))))

              ("literature"
               :keys "l"
               :file (lambda () (read-file-name "file: " z-org-literature-notes-dir))
               :children (("add to readlist"
                           :keys "a"
                           :file ,(file-name-concat z-org-literature-dir "readlist.org")
                           :headline "inbox"
                           :prepend t
                           :template ("* [ ] %^{title}%? %^g"))

                          ("init source"
                           :keys "i"
                           :file (lambda ()
                                   (file-name-concat z-org-literature-notes-dir (concat (replace-regexp-in-string " "
                                                                                                                  "_"
                                                                                                                  (read-from-minibuffer "short title: "))
                                                                                        ".org")))
                           :type plain
                           :template ("#+title:  %^{full title}"
                                      "#+author: %(user-full-name)"
                                      "#+email:  %(message-user-mail-address)"
                                      "#+date:   %<%F>"
                                      "#+filetags: :literature:%^g"
                                      ""
                                      "* [-] %\\1%?"
                                      ":PROPERTIES:"
                                      ":title:  %\\1"
                                      ":author: %^{author}"
                                      ":year:   %^{year}"
                                      ":type:   %^{ |book|textbook|book|paper|article|audiobook|podcast}"
                                      ":pages:  %^{pages}"
                                      ":END:")
                           :hook (lambda () (message "change task-state in readlist.org!")))

                          ("quote"
                           :keys "q"
                           :headline "quotes"
                           :empty-lines-before 1
                           :template ("* %^{title} [p.%^{page}]"
                                      ":PROPERTIES:"
                                      ":created: %U"
                                      ":END:"
                                      "#+begin_quote"
                                      "%?"
                                      "#+end_quote"))

                          ("note: literary"
                           :keys "l"
                           :headline "literature notes"
                           :empty-lines-before 1
                           :template ("* %^{title} [p.%^{page}] %^g"
                                      ":PROPERTIES:"
                                      ":created: %U"
                                      ":END:"
                                      "%?"))

                          ("note: transient"
                           :keys "t"
                           :headline "transient notes"
                           :empty-lines-before 1
                           :template ("* %^{title} %^g"
                                      ":PROPERTIES:"
                                      ":created: %U"
                                      ":END:"
                                      "%?"))

                          ("summarize"
                           :keys "s"
                           :headline "summary"
                           :unnarrowed t
                           :type plain
                           :template ("%?")
                           :hook (lambda ()
                                   (message "change task-state!: TODO -> DONE")))))))) ;; in order to log finishing date
;; capture templates:1 ends here

;; [[file:config.org::*agenda][agenda:1]]
(add-hook! 'org-agenda-mode-hook #'org-super-agenda-mode)

(setq org-archive-location "~/Archive/org/%s::" ;; NOTE :: archive based on file path
      org-agenda-files `(,@(directory-files-recursively org-directory org-agenda-file-regexp t)
                         ,(z-doct-journal-file)
                         ,(z-doct-journal-file (time-subtract (current-time) (days-to-time 1)))) ;; include tasks from {today's, yesterday's} journal's agenda
      org-agenda-skip-scheduled-if-done t
      ;; org-agenda-sticky t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-tags-column 0
      org-agenda-block-separator ?─
      org-agenda-breadcrumbs-separator "…"
      org-agenda-compact-blocks nil
      org-agenda-show-future-repeats nil
      org-deadline-warning-days 3
      org-agenda-time-grid nil
      org-capture-use-agenda-date t)

(defadvice! z-add-newline (fn &rest args)
  "Separate dates in 'org-agenda' with newline."
  :around #'org-agenda-format-date-aligned
  (concat "\n" (apply fn args) ))
;; agenda:1 ends here

;; [[file:config.org::*agenda][agenda:2]]
(setq org-agenda-todo-keyword-format "%-3s"
      org-agenda-scheduled-leaders '(""
                                     "<< %1dd") ;; NOTE :: unicode is not fixed width => breaks formatting => cannot use it.
      org-agenda-deadline-leaders '("─────"
                                    ">> %1dd"
                                    "<< %1dd")
      org-agenda-prefix-format '((agenda . "%-20c%-7s%-7t") ;; note all columns separated by minimum 2 spaces
                                 (todo   . "%-20c%-7s%-7t")
                                 (tags   . "%-20c%-7s%-7t")
                                 (search . "%-20c%-7s%-7t")))
;; agenda:2 ends here

;; [[file:config.org::*org roam][org roam:1]]
(setq org-roam-directory z-wiki-dir)
;; org roam:1 ends here

;; [[file:config.org::*end org][end org:1]]
)
;; end org:1 ends here

;; [[file:config.org::*latex][latex:1]]
(setq +latex-viewers '(zathura))
;; latex:1 ends here

;; [[file:config.org::*verilog][verilog:1]]
(after! verilog-mode
  (setq verilog-auto-newline nil))

(setq-hook! 'verilog-mode-hook
  verilog-case-indent z-indent-width
  verilog-cexp-indent z-indent-width
  verilog-indent-level z-indent-width
  verilog-indent-level-behavioral z-indent-width
  verilog-indent-level-declaration z-indent-width
  verilog-indent-level-module z-indent-width)
(map! :after verilog-mode :map verilog-mode-map :localleader
      "cf" #'verilog-indent-buffer) ;; code:format
;; verilog:1 ends here

;; [[file:config.org::*dictionary][dictionary:1]]
(after! dictionary
  (setq dictionary-server "dict.org"
        dictionary-default-dictionary "*"))
;; dictionary:1 ends here

;; [[file:config.org::*devdocs][devdocs:1]]
(setq-hook! 'java-mode-hook devdocs-current-docs '("openjdk~17"))
(setq-hook! 'ruby-mode-hook devdocs-current-docs '("ruby~3.3"))
;; devdocs:1 ends here

;; [[file:config.org::*speech notes dictation: whisper][speech notes dictation: whisper:1]]
(use-package whisper
  :load-path "~/.config/doom/whisper.el/")
;; speech notes dictation: whisper:1 ends here
