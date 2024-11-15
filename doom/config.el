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

  ;; list of customizeable faces: `(helpful-variable 'modus-themes-faces)`
  (custom-set-faces!
    '(org-list-dt :inherit modus-themes-heading-1)
    `(org-block-begin-line :foreground ,(modus-themes-get-color-value 'prose-metadata))
    '(org-quote :slant italic))

  (setq doom-theme 'modus-operandi))
;; modus-theme:1 ends here

;; [[file:config.org::*font][font:1]]
(setq doom-font                (font-spec :family "Iosevka Comfy" :size 13)
      doom-variable-pitch-font (font-spec :family "Noto Serif" :size 15)
      doom-serif-font          (font-spec :family "Iosevka Comfy" :size 13)
      doom-big-font            (font-spec :family "Iosevka Comfy" :size 22)
      doom-font-increment 1)

(map! :n "C-0" #'doom/reset-font-size)
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
      split-width-threshold 100) ;; force vsplits, not more than 2 windows

(after! org
  (setq org-src-window-setup 'current-window
        org-agenda-window-setup 'current-window))

(add-hook 'org-capture-mode-hook #'delete-other-windows)

(setq display-buffer-alist `(("^\\*\\(Org Src\\|Org Agenda\\)";; edge-case *buffers* that i treat as master buffers
                              (display-buffer-same-window))
                             ("^\\*" ;; all slave *buffers*
                              (display-buffer-in-side-window) ;; make slave buffers appear as vertical split to right of master buffer
                              (side . right)
                              (window-width . 0.5) ;; equal 2 window split
                              (slot . 0))))

(define-key! [remap doom/open-scratch-buffer] #'doom/switch-to-scratch-buffer) ;; open scratch in fullscreen, not popup
;; window layout & behavior:1 ends here

;; [[file:config.org::*window layout & behavior][window layout & behavior:2]]
(add-hook! '(dired-mode-hook
             text-mode-hook
             conf-mode-hook
             Info-mode-hook
             org-agenda-mode-hook
             nov-mode-hook
             magit-mode-hook)
           #'visual-fill-column-mode)

(setq-default visual-fill-column-enable-sensible-window-split t
              visual-fill-column-center-text t
              visual-fill-column-width 100
              fill-column 100)
;; window layout & behavior:2 ends here

;; [[file:config.org::*general options][general options:1]]
(setq initial-scratch-message ""
      delete-by-moving-to-trash t
      bookmark-default-file "~/.config/doom/bookmarks" ;; save bookmarks in config dir (preserve for newinstalls)
      auto-save-default t
      confirm-kill-emacs nil
      hscroll-margin 0
      scroll-margin 0
      next-screen-context-lines 0 ;; no confusing page overlaps, always start reading on the first visible line of the next page
      enable-recursive-minibuffers nil
      display-line-numbers-type 'visual
      shell-command-prompt-show-cwd t
      async-shell-command-width 100
      shell-file-name (executable-find "fish")) ;; we use fish-shell os-wide!

(+global-word-wrap-mode 1)
(add-hook! 'compilation-mode-hook #'+word-wrap-mode) ;; HACK :: must enable again

(save-place-mode 1)
(global-subword-mode 1)
(add-hook! prog-mode-hook #'rainbow-delimiters-mode)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)
;; general options:1 ends here

;; [[file:config.org::*leader (\[\[kbd:SPC\]\[SPC\]\], \[\[kbd:,\]\[,\]\])][leader ([[kbd:SPC][SPC]], [[kbd:,][,]]):1]]
(setq doom-leader-key "SPC"
      doom-leader-alt-key "C-SPC"
      doom-localleader-key ","
      doom-localleader-alt-key "C-,")

(map! :leader
      "." #'vertico-repeat
      "'" #'consult-bookmark
      "X" #'whisper-run
      (:prefix "h"
               "w" #'tldr)
      (:prefix "s"
               "k" #'devdocs-lookup
               "t" #'dictionary-search)
      (:prefix "f"
               "f" #'+vertico/consult-fd-or-find
               "F" (cmd! (call-interactively #'find-file)
                         (+vertico/consult-fd-or-find)))
      (:prefix "c"
               "r" #'lsp-rename
               (:prefix "'"
                        "t" #'org-babel-tangle
                        "T" #'org-babel-detangle))
      (:prefix "n"
               "g" #'org-capture-goto-last-stored)
      (:prefix "t"
               "c" #'visual-fill-column-mode))
;; leader ([[kbd:SPC][SPC]], [[kbd:,][,]]):1 ends here

;; [[file:config.org::*global navigation scheme][global navigation scheme:1]]
(map! :map 'override
      :nm "C-w"     #'next-window-any-frame
      :nm "C-q"     #'evil-window-delete ;; dwim
      :nm "C-s"     #'basic-save-buffer ;; statistically most called command => ergonomic (& default) mapping
      :nm "C-f"     #'find-file
      :nm "C-b"     #'consult-buffer
      :nm "C-<tab>" #'evil-switch-to-windows-last-buffer)
;; global navigation scheme:1 ends here

;; [[file:config.org::*global marks][global marks:1]]
(define-key! [remap evil-goto-mark-line] #'z-evil-goto-mark-buffer)
(map! :map pdf-view-mode-map :n "`" #'pdf-view-jump-to-register)

(evil-define-command z-evil-goto-mark-buffer (char &optional noerror)
  "Go to the global-marker's buffer specified by CHAR.

This differs from `evil-goto-mark-line' in that it does not actually go to the marked position, which
is undesired, since we use this for switching inbetween buffers and don't want our position to get
reset each time.

for ergonomics and speed you can input the mark as lowercase (vim uses UPPERCASE marks)."
  :repeat nil
  :jump t
  (interactive (list (read-char)))
  (let ((marker (evil-get-marker (upcase char))))
    (cond
     ((markerp marker)
      (switch-to-buffer (marker-buffer marker)))
     ((numberp marker) nil)             ;; already in buffer
     ((consp marker)
      (if (find-buffer-visiting (car marker))
          (switch-to-buffer (find-buffer-visiting (car marker)))
        (find-file (car marker))))
     ((not noerror)
      (user-error "global marker `%c' is not set" (upcase char))))))

;; HACK :: make evil's global markers persist across sessions (save state => reduce repetition, increase consistency)
(after! savehist
  (add-to-list 'savehist-additional-variables 'evil-markers-alist)
  (add-hook! 'savehist-save-hook
    (kill-local-variable 'evil-markers-alist)
    (dolist (entry evil-markers-alist)
      (when (markerp (cdr entry))
        (setcdr entry (cons (file-truename (buffer-file-name (marker-buffer (cdr entry))))
                            (marker-position (cdr entry)))))))
  (add-hook! 'savehist-mode-hook
    (setq-default evil-markers-alist evil-markers-alist)
    (kill-local-variable 'evil-markers-alist)
    (make-local-variable 'evil-markers-alist)))
;; global marks:1 ends here

;; [[file:config.org::*vim editing][vim editing:1]]
(map! :after evil
      :nmv "C-i" #'better-jumper-jump-forward ;; HACK :: fix overridden binding
      :nv "S-<return>" #'newline-and-indent
      :nm "g/"  #'occur

      :nv "+"   #'evil-numbers/inc-at-pt ;; more sensible than `C-x/C-a', `+-' in vim is useless
      :nv "-"   #'evil-numbers/dec-at-pt
      :nv "g+"  #'evil-numbers/inc-at-pt-incremental
      :nv "g-"  #'evil-numbers/dec-at-pt-incremental

      :nv "g<"  #'evil-lion-left
      :nv "g>"  #'evil-lion-right

      :nv "&"   #'query-replace-regexp
      :nv "s"   #'evil-surround-region
      :nv "S"   #'evil-Surround-region)

(define-key! [remap evil-next-line] #'evil-next-visual-line)
(define-key! [remap evil-previous-line] #'evil-previous-visual-line)

(define-key! [remap evil-ex] #'execute-extended-command) ;; burn vim's bridges and harness power of emacs

(define-key key-translation-map (kbd "C-h") (kbd "DEL")) ;; HACK :: simulate `C-h' as backspace consistently (some modes override it to `help').
;; vim editing:1 ends here

;; [[file:config.org::*completion][completion:1]]
(map! :map company-mode-map :after company
      :i "C-n" #'company-complete)

(map! :map minibuffer-mode-map
      :i "C-n" #'completion-at-point
      :n "k"   #'previous-line-or-history-element ;; navigate history in normal mode
      :n "j"   #'next-line-or-history-element
      :n "/"   #'previous-matching-history-element
      :n "<return>" #'exit-minibuffer) ;; sane default

(map! :map vertico-flat-map :after vertico
      :i "C-n" #'next-line-or-history-element  ;; navigate elements like vim completion (and consistent with the os)
      :i "C-p" #'previous-line-or-history-element
      :n "k"   #'previous-line-or-history-element ;; navigate history in normal mode
      :n "j"   #'next-line-or-history-element
      :n "<return>" #'vertico-exit ;; sane default
      :n "/"   #'previous-matching-history-element)

(map! :map evil-ex-search-keymap :after evil
      :n "j" #'next-line-or-history-element
      :n "k" #'previous-line-or-history-element
      :n "/" #'previous-matching-history-element
      :n "<return>" #'exit-minibuffer)

(map! :map vertico-map
      :im "C-w" #'vertico-directory-delete-word
      :im "C-d" #'consult-dir
      :im "C-f" #'consult-dir-jump-file)
;; completion:1 ends here

;; [[file:config.org::*lispy(ville)][lispy(ville):1]]
(after! lispy
  (setq lispy-key-theme '(lispy c-digits)))
;; lispy(ville):1 ends here

;; [[file:config.org::*evil-mode][evil-mode:1]]
(evil-surround-mode 1)
(after! evil
  (setq evil-want-fine-undo nil
        evil-ex-substitute-global t
        evil-want-C-i-jump t
        evil-want-C-h-delete t
        evil-want-minibuffer t ;; don't loose your powers in the minibuffer
        evil-org-use-additional-insert nil)
  (add-to-list 'evil-normal-state-modes 'shell-mode) ;; normal mode by default :: 99% of the time i want to navigate the compilation/shell buffer.  (and not read stdin))
  (add-to-list 'evil-surround-pairs-alist '(?` . ("`" . "`")))

  (defadvice! z-default-last-register (fn &rest args)
    "when a macro is recorded and `evil-last-register' is still `nil' (no macro executed before), set it to the just recorded macro.
  which is the sane default behaviour allowing you to: record a macro with `qq' and immediately call it with `@@', instead of getting an error and having to retype `@q' again."
    :after #'evil-record-macro
    (when (not evil-last-register)
      (setq evil-last-register evil-last-recorded-register)))

  (defadvice! z-update-evil-search-reg (fn &rest args)
    "Update evil search register after jumping to a line with
`+default/search-buffer' to be able to jump to next/prev matches.
This is sensible default behaviour, and integrates it into evil."
    :after #'+default/search-buffer
    (let ((str (--> nil
                    (car consult--line-history)
                    (string-replace " " ".*" it))))
      (push str evil-ex-search-history)
      (setq evil-ex-search-pattern (list str t t)))))

(defadvice! z-save-excursion (fn &rest args)
  "when modifying the buffer with one of these functions, do the edit and then  restore point to where it was originally."
  :around '(query-replace-regexp
            query-replace
            +format:region)
  (save-excursion
    (apply fn args)))

(advice-add '+fold/previous :override #'ignore) ;; FIXME :: `+fold/previous` disabled, since it crashes emacs. (don't call it by accident via binding)

;; make evil's global markers persist across sessions (save state => reduce repetition, increase consistency)
(after! savehist
  (add-to-list 'savehist-additional-variables 'evil-markers-alist)
  (add-hook! 'savehist-save-hook
    (kill-local-variable 'evil-markers-alist)
    (dolist (entry evil-markers-alist)
      (when (markerp (cdr entry))
        (setcdr entry (cons (file-truename (buffer-file-name (marker-buffer (cdr entry))))
                            (marker-position (cdr entry)))))))
  (add-hook! 'savehist-mode-hook
    (setq-default evil-markers-alist evil-markers-alist)
    (kill-local-variable 'evil-markers-alist)
    (make-local-variable 'evil-markers-alist)))
;; evil-mode:1 ends here

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

;; [[file:config.org::*dired][dired:1]]
(after! dired
  (add-hook! 'dired-mode-hook #'dired-hide-details-mode) ;; less clutter (enable manually if needed)
  (setq dired-open-extensions (mapcan (lambda (pair)
                                        (let ((extensions (car pair))
                                              (app (cdr pair)))
                                          (mapcar (lambda (ext)
                                                    (cons ext app))
                                                  extensions)))
                                      '((("mkv" "webm" "mp4" "mp3") . "mpv")
                                        (("gif" "jpeg" "jpg" "png") . "nsxiv")
                                        (("docx" "odt" "odf")       . "libreoffice")))
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-no-confirm '(uncompress move copy)
        dired-omit-files "^\\..*$"))
;; dired:1 ends here

;; [[file:config.org::*keybindings][keybindings:1]]
(map! :map dired-mode-map :after dired
      :m "h" #'dired-up-directory
      :m "l" #'dired-open-file)

(map! :map dired-mode-map :localleader :after dired
      :m "a" #'z-dired-archive)
;; keybindings:1 ends here

;; [[file:config.org::*archive file][archive file:1]]
(defvar z-archive-dir "~/Archive/")

(defun z-dired-archive ()
  "`mv' marked file/s to: `z-archive-dir'/{relative-filepath-to-HOME}/{filename}"
  (interactive)
  (mapc (lambda (file)
          (let* ((dest (--> file
                            (file-relative-name it "~/")
                            (file-name-concat z-archive-dir it)))
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
              org-indent-indentation-per-level z-indent-width
              evil-indent-convert-tabs t
              indent-tabs-mode nil)

(setq-hook! '(c++-mode-hook
              c-mode-hook
              java-mode-hook)
  tab-width z-indent-width
  c-basic-offset z-indent-width
  evil-shift-width z-indent-width)

(setq-hook! 'ruby-mode-hook
  evil-shift-width z-indent-width
  ruby-indent-level z-indent-width)
;; indentation:1 ends here

;; [[file:config.org::*org][org:1]]
(after! org
;; org:1 ends here

;; [[file:config.org::*options][options:1]]
(add-hook! 'org-mode-hook '(visual-line-mode
                            org-fragtog-mode
                            rainbow-mode
                            laas-mode
                            +org-pretty-mode
                            org-appear-mode))

(setq-hook! 'org-mode-hook warning-minimum-level :error) ;; prevent frequent popups of *warning* buffer

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
      org-blank-before-new-entry '((heading . nil)
                                   (plain-list-item . nil))
      org-src-ask-before-returning-to-edit-buffer nil)

(defadvice! z-insert-newline-above (fn &rest args)
  "pad newly inserted heading with newline unless is todo-item.

  since i often have todolists , where i don't want the newlines.  newlines are for headings that have a body of text."
  :after #'+org/insert-item-below
  (when (and (org-at-heading-p)
             (not (org-entry-is-todo-p)))
    (+evil/insert-newline-above 1)))

(defadvice! z-insert-newline-below (fn &rest args)
  :after #'+org/insert-item-above
  (when (and (org-at-heading-p)
             (not (org-entry-is-todo-p)))
    (+evil/insert-newline-below 1)))
;; options:1 ends here

;; [[file:config.org::*symbols][symbols:1]]
(add-hook! 'org-mode-hook '(org-superstar-mode
                            prettify-symbols-mode))

(setq org-superstar-headline-bullets-list "●")

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

;; [[file:config.org::*keybindings][keybindings:1]]
(map! :map org-mode-map :after org
      :localleader
      "\\" #'org-latex-preview
      ","  #'org-ctrl-c-ctrl-c
      "z"  #'org-add-note
      "["  :desc "toggle-checkbox" (cmd! (let ((current-prefix-arg 4))
                                           (call-interactively #'org-toggle-checkbox))))
;; keybindings:1 ends here

;; [[file:config.org::*babel][babel:1]]
(setq org-babel-default-header-args '((:session  . "none")
                                      (:results  . "replace")
                                      (:exports  . "code")
                                      (:cache    . "yes")
                                      (:noweb    . "yes")
                                      (:hlines   . "no")
                                      (:tangle   . "no")
                                      (:mkdirp   . "yes")
                                      (:comments . "link"))) ;; important for when wanting to retangle
;; babel:1 ends here

;; [[file:config.org::*clock][clock:1]]
(setq org-clock-out-when-done t
      org-clock-persist t
      org-clock-into-drawer t)
;; clock:1 ends here

;; [[file:config.org::*task states][task states:1]]
;; ! => save timestamp on statchange
;; @ => save timestamp on statchange & add note associated with change to LOG.
(setq org-todo-keywords '((sequence
                           "[ ](t)"
                           "[@](e)"
                           "[?](?!)"
                           "[-](-@)"
                           "[>](>@)"
                           "[=](=@)"
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
      org-log-into-drawer "LOG") ;; more concise & modern than: LOGBOOK

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

;; [[file:config.org::*capture templates][capture templates:1]]
(setq org-directory "~/Documents/org/")

(defvar z-org-journal-dir (file-name-concat "~/Documents/journal/")
  "dir for daily captured journal files")

(defvar z-org-literature-dir "~/Documents/literature"
  "literature sources and captured notes")

(defvar z-org-literature-notes-dir (file-name-concat z-org-literature-dir "notes/")
  "note files for each literature source")

(defvar z-wiki-dir "~/Documents/wiki/"
  "personal knowledge base directory :: cohesive, structured, standalone articles/guides.
(blueprints and additions to these articles are captured into 'org-directory/personal/notes.org',
and the later reviewed and merged into the corresponding article of the wiki.")

(defvar z-doct-default-templates '(z-doct-task-template
                                   z-doct-event-template
                                   z-doct-note-template))

(defvar z-doct-projects `(("cs" :keys "c"
                           :templates ,z-doct-default-templates
                           :children (("ti"   :keys "t")
                                      ("an2"  :keys "a")
                                      ("ph1"  :keys "p")
                                      ("spca" :keys "s" :templates (z-doct-cc-src-template))
                                      ("nm"   :keys "n" :templates (z-doct-cc-src-template))))
                          ("personal" :keys "p" :templates ,z-doct-default-templates)
                          ("config"   :keys "f" :templates ,z-doct-default-templates))
  "- same syntax as doct,  except for the key-value-pair: `:templates LIST`,
 where LIST is a list of functions with signature: `(FN PATH) -> TEMPLATE`
 where PATH is to be generated by z-doct-projects file
 where TEMPLATE is a valid `doct-capture-template`.
 - `:templates` is inherited by the parent-group and if present in a childgroup it appends the additionally defined templates.")

(defun z-doct-journal-file (&optional time)
  "returns a structured filename based on the current date.
eg: 2024-11-03_journal.org
TIME :: time in day of note to return. (default: today)"
  (--> nil
       (or time (current-time))
       (format-time-string "%F" it)
       (format "%s_journal.org" it)
       (file-name-concat z-org-journal-dir it)))

(defun z-doct-projects-file (type path)
  "TYPE :: 'agenda | 'notes"
  (--> nil
       (symbol-name type)
       (format "%s.org" it)
       (file-name-concat org-directory path it)))

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

(defun z-doct-cc-src-template (path)
  "for quickly implementing/testing ideas (like a scratchpad, but you have all your experimentations
  in a single literate document).  choose either c or c++"
  (list "note: src cc"
        :keys "s"
        :file (z-doct-projects-file 'notes path)
        :prepend t
        :empty-lines 1
        :template '("* %^{title} :%^{lang|C|C|cpp}:"
                    ":PROPERTIES:"
                    ":created: %U"
                    ":END:"
                    "#+begin_src %\\2"
                    "<<%\\2_header>>" ;; <<header>> is org-babel's `:noweb` syntax and the named org-src-block: `c_header` (or cpp_header) (which must be present in the targetfile.  depending on wether the project uses C or cpp it is different) and should contains stuff like `#include <iostream>' that is basically needed for every single snippet.
                    ""
                    "int main() {"
                    "        %?"
                    "}"
                    "#+end_src")))

(defun z-doct-expand-templates (projects &optional inherited-templates parent-path)
  "PROJECTS :: `z-doct-projects'
PARENT-PATH :: nil (used for recursion) "
  (mapcar (lambda (project)
            (let* ((tag (car project))
                   (props (cdr project))
                   (key (plist-get props :keys))
                   (self `(,tag :keys ,key))
                   (children (plist-get props :children))
                   (templates (append inherited-templates (plist-get props :templates)))
                   (path (file-name-concat parent-path tag)))
              (append self
                      (if children
                          (--> nil ;; HAS CHILDREN => is project-node => recursivly expand children
                               (list self)
                               (z-doct-expand-templates it templates) ;; template out of self
                               (append it (z-doct-expand-templates children templates path))
                               (list :children it))
                        (--> nil ;; NO CHILDREN => is leaf-node => instantiate templates
                             (mapcar (lambda (fn-sym)
                                       (funcall fn-sym path))
                                     templates)
                             (list :children it))))))
          projects))

(setq org-capture-templates
      (doct `(;; PROJECT TEMPLATES
              ,@(z-doct-expand-templates z-doct-projects)

              ;; NON-PROJECT TEMPLATES
              ("journal"
               :keys "j"
               :file (lambda () (z-doct-journal-file))
               :title (lambda ()
                        (--> nil
                             (format-time-string "journal: %A, %e. %B %Y")
                             (downcase it)))

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
                                       "** [ ] "
                                       ""
                                       "* notes"))

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
                                   (--> nil
                                        (time-subtract (current-time) (days-to-time 1))
                                        (z-doct-journal-file it)))
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
                           :template ("* [ ] %^{title}"))

                          ("init source"
                           :keys "i"
                           :file (lambda ()
                                   (--> nil
                                        (read-from-minibuffer "short title: ")
                                        (replace-regexp-in-string " " "_" it)
                                        (concat it ".org")
                                        (file-name-concat z-org-literature-notes-dir it)))
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
                                      ":type:   %^{type|book|book|textbook|book|paper|article|audiobook|podcast}"
                                      ":pages:  %^{pages}"
                                      ":END:")
                           :hook (lambda () (message "change task-state in readlist.org!")))

                          ("quote"
                           :keys "q"
                           :headline "quotes"
                           :empty-lines-before 1
                           :template ("* %^{title} [pg: %^{page}]"
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
                           :template ("* %^{title} [pg: %^{page}] %^g"
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

(setq org-archive-location (--> nil
                                (string-remove-prefix "~/" org-directory)
                                (file-name-concat "~/Archive/" it "%s::")) ;; NOTE :: archive based on file path
      org-agenda-files (append (directory-files-recursively org-directory org-agenda-file-regexp t)
                               (list (z-doct-journal-file)
                                     (--> nil
                                          (time-subtract (current-time) (days-to-time 1))
                                          (z-doct-journal-file it)))) ;; include tasks from {today's, yesterday's} journal's agenda
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

;; [[file:config.org::*dictionary][dictionary:1]]
(after! dictionary
  (setq dictionary-server "dict.org"
        dictionary-default-dictionary "*"))
;; dictionary:1 ends here

;; [[file:config.org::*devdocs][devdocs:1]]
;; unfortunately using cl-loop/mapcar/dolist don't work...
(setq-hook! 'java-mode-hook devdocs-current-docs '("openjdk~17"))
(setq-hook! 'ruby-mode-hook devdocs-current-docs '("ruby~3.3"))
(setq-hook! 'c++-mode-hook devdocs-current-docs '("cpp" "eigen3"))
(setq-hook! 'c-mode-hook devdocs-current-docs '("c"))
;; devdocs:1 ends here

;; [[file:config.org::*whisper: transcription][whisper: transcription:1]]
(evil-define-operator z-reformat-prose (beg end)
  "we write all lowercase, all the time (to make the text more monotone, such that it's value will
speak more for it's self).  using the technical document convention of double space full stops for
legibility."
  (save-excursion
      (downcase-region beg end)
      (repunctuate-sentences t beg end)))

(add-hook! 'whisper-after-transcription-hook (z-reformat-prose (point-min) (point-max)))
;; whisper: transcription:1 ends here

;; [[file:config.org::*harpoon][harpoon:1]]
(after! harpoon
  (setq harpoon-cache-file "~/.local/share/emacs/harpoon/") ;; HACK :: move it out of '.config', since '.config' has a git repo (harpoon interprets it as project => harpooning in harpoonfile will use the harpoonfile of project: '.config' instead of currently-opened harpoonfile).

  (map! 'override
      :nm "M-1"     #'harpoon-go-to-1
      :nm "M-2"     #'harpoon-go-to-2
      :nm "M-3"     #'harpoon-go-to-3
      :nm "M-4"     #'harpoon-go-to-4
      :nm "M"       #'harpoon-add-file) ;; quickly add file to harpoon

(map! :leader
      "m" #'harpoon-toggle-file)) ;; for deleting and reordering harpoon candidates
;; harpoon:1 ends here

;; [[file:config.org::*vertico: minibuffer completion][vertico: minibuffer completion:1]]
(vertico-flat-mode 1)
;; vertico: minibuffer completion:1 ends here

;; [[file:config.org::*company: code completion][company: code completion:1]]
(after! company
  (setq company-minimum-prefix-length 0
        company-idle-delay nil ;; only show menu when explicitly activated
        company-show-quick-access t
        company-global-modes '(not
                               help-mode
                               eshell-mode
                               org-mode
                               vterm-mode)))
;; company: code completion:1 ends here

;; [[file:config.org::*nov: ebooks][nov: ebooks:1]]
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width t) ;; used visual-line-mode and visual-fill-column mode to visually wrap line.
  (advice-add 'nov-render-title :override #'ignore) ;; using modeline...

  (map! :map (nov-mode-map nov-button-map)
        "SPC" nil                     ;; never override leader-mode
        "S-SPC" nil                   ;; never override leader-mode
        :n "q" #'kill-current-buffer ;; consistent with other read-only modes (magit, dired, docs...)

        :n "<next>" #'nov-scroll-up  ;; main scrolling methods
        :n "<prior>" #'nov-scroll-down)

  (add-hook! 'nov-mode-hook
    (setq-local line-spacing 2 ;; padding increases focus on current line for long prose text.
                global-hl-line-mode nil) ;; HACK :: need to unset, instead of using a hook   ;; HACK :: ---
    (visual-line-mode 1)))
;; nov: ebooks:1 ends here

;; [[file:config.org::*pdf view][pdf view:1]]
(map! :map pdf-view-mode-map :after (pdf-view pdf-history pdf-outline)
      :n "p" #'pdf-view-fit-page-to-window
      :n "w" #'pdf-view-fit-width-to-window
      :n "<tab>" #'pdf-outline) ;; consistent with (org mode, magit, etc) :: using <tab> for "OVERVIEW"

(define-key! [remap pdf-view-scale-reset] #'pdf-view-fit-page-to-window) ;; view fit-page fit as reset.
;; pdf view:1 ends here

;; [[file:config.org::*yas: snippets][yas: snippets:1]]
(setq yas-triggers-in-field t)
;; yas: snippets:1 ends here

;; [[file:config.org::*file templates][file templates:1]]
(set-file-templates!
 '(org-mode :trigger "header")
 '(prog-mode :trigger "header")
 '(makefile-gmake-mode :ignore t))
;; file templates:1 ends here
