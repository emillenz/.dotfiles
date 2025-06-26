;; -*- lexical-binding: t -*-

;; ---
;; title: minimalist, distractionless vanilla emacs config
;; date: [2025-05-09]
;; author: emil lenz
;; email: emillenz@protonmail.com
;; ---

(use-package use-package
  :config
  (setopt use-always-demand t
          use-package-enable-imenu-support t))

(use-package package
  :config
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

(use-package emacs
  :config
  (global-auto-revert-mode)
  (global-visual-line-mode)
  (column-number-mode)
  (global-subword-mode)
  (electric-indent-mode)
  (electric-pair-mode)
  (save-place-mode)
  (auto-save-visited-mode)
  (kill-ring-deindent-mode)
  (recentf-mode)
  (fringe-mode 0)

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (scroll-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (tooltip-mode -1)

  (setopt user-full-name "emil lenz"
	  user-mail-address "emillenz@protonmail.com")

  (setopt uniquify-buffer-name-style 'forward
	  delete-by-moving-to-trash t
	  remote-file-name-inhibit-delete-by-moving-to-trash t
	  ring-bell-function 'ignore
	  enable-recursive-minibuffers t
	  revert-without-query '(".*\\.pdf")
	  global-auto-revert-non-file-buffers t
	  auto-save-include-big-deletions t
	  kill-buffer-delete-auto-save-files t
	  custom-file (file-name-concat user-emacs-directory "custom.el")
	  use-short-answers t
	  save-interprogram-paste-before-kill t
	  require-final-newline t
	  load-prefer-newer t
	  desktop-dirname user-emacs-directory
	  find-file-visit-truename t
	  comment-empty-lines nil
	  show-paren-when-point-inside-paren t
	  kill-do-not-save-duplicates t
	  shift-select-mode nil
	  kmacro-execute-before-append nil
	  vc-follow-symlinks t
	  pulse-flag 'default
	  mark-ring-max 64)

  (setopt history-length 1000
          history-delete-duplicates t)

  (setopt shell-command-prompt-show-cwd t
          async-shell-command-display-buffer nil
          async-shell-command-buffer 'new-buffer)

  (setopt create-lockfiles nil
          make-backup-files nil
          delete-old-versions t
          version-control t
          kept-new-versions 5
          kept-old-versions 5
          vc-make-backup-files nil
          backup-by-copying t
          backup-by-copying-when-linked t
          backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

  (setopt initial-scratch-message nil
          inhibit-startup-echo-area-message user-login-name
          initial-buffer-choice nil
          inhibit-splash-screen t
          inhibit-startup-screen t
          inhibit-startup-buffer-menu t
          frame-title-format "%b ― emacs"
          icon-title-format frame-title-format
          frame-inhibit-implied-resize t
          use-file-dialog nil
          use-dialog-box nil)

  (setopt scroll-preserve-screen-position t
          scroll-error-top-bottom t
          auto-window-vscroll nil)

  (setopt compilation-scroll-output t
          next-error-recenter '(4))

  (progn
    (setopt tab-always-indent t
            indent-tabs-mode t
	    standard-indent 8
	    tab-width standard-indent)

    (setopt c-default-style "linux")
    (with-eval-after-load 'js (setopt js-indent-level standard-indent))
    (with-eval-after-load 'sh-script (setopt sh-basic-offset standard-indent)))

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (progn
    (setopt minibuffer-prompt-properties
            '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode))

  (progn
    (set-language-environment "utf-8")
    (setopt default-input-method nil))

  (put 'narrow-to-region 'disabled nil)

  (advice-add 'mark-paragraph
              :before
              (lambda (&rest _)
                (unless (or (use-region-p)
                            (eq last-command this-command)
                            (when (mark) (= (mark) (point))))
                  (push-mark))))

  (advice-add 'yank
	      :after
	      (lambda (&rest _)
		(call-interactively 'indent-region)))

  (seq-do (lambda (cmd)
	    (advice-add cmd :around
                        (lambda (fn &rest args)
                          (with-undo-amalgamate
                            (apply fn args)))))
	  '(kmacro-call-dwim
            query-replace-regexp))

  (progn
    (defmacro keymap-set! (keymap &rest pairs)
      (macroexp-progn
       (cl-loop for (key cmd)
		on pairs
		by 'cddr
		collect `(keymap-set ,keymap ,key ,cmd))))

    (setopt kill-whole-line t)

    (keymap-set! global-map
		 "<remap> <keyboard-quit>" 'keyboard-escape-quit
		 "<remap> <downcase-word>" 'downcase-dwim
		 "<remap> <upcase-word>" 'upcase-dwim
		 "<remap> <capitalize-word>" 'capitalize-dwim
		 "<remap> <delete-horizontal-space>" 'cycle-spacing
		 "<remap> <kill-buffer>" 'kill-current-buffer

		 "M-SPC" 'mark-word

		 "C-M-{" 'beginning-of-defun
		 "C-M-}" 'end-of-defun

		 "<remap> <open-line>"
		 (defun open-line-dwim ()
		   (interactive)
		   (if (eq (point) (save-excursion
				     (back-to-indentation)
				     (point)))
		       (call-interactively 'split-line)
		     (save-excursion
		       (call-interactively 'default-indent-new-line))))

		 "<remap> <yank>"
		 (defun yank-dwim (&optional arg)
		   (interactive "P")
		   (let ((text (when (use-region-p)
				 (delete-and-extract-region (region-beginning)
							    (region-end)))))
		     (yank arg)
		     (when text (kill-new text))))

		 "<remap> <comment-dwim>"
		 (defun comment-sexp-dwim (&optional beg end arg)
		   (interactive "r\nP")
		   (unless (use-region-p)
		     (let ((bounds (save-excursion (forward-sexp)
						   (backward-sexp)
						   (bounds-of-thing-at-point 'sexp))))
		       (setq beg (car bounds)
			     end (cdr bounds))))
		   (comment-or-uncomment-region beg end arg))

		 "<remap> <eval-last-sexp>"
		 (defun eval-last-sexp-dwim (&optional arg)
		   (interactive "P")
		   (if (use-region-p)
		       (progn
			 (eval-region (region-beginning)
				      (region-end)
				      standard-output)
			 (deactivate-mark))
		     (call-interactively 'eval-last-sexp)))

		 "<remap> <kmacro-end-and-call-macro>"
		 (defun kmacro-call-dwim ()
		   (interactive)
		   (if (use-region-p)
		       (progn
			 (call-interactively 'apply-macro-to-region-lines)
			 (deactivate-mark))
		     (call-interactively 'kmacro-end-and-call-macro))))

    (keymap-set! ctl-x-map
		 "M-h" 'mark-end-of-sentence
		 "f" 'recentf-open
		 "C-z" 'shell

		 "C-b"
		 (defun switch-to-other-buffer ()
		   (interactive)
		   (let ((buf (caar (window-prev-buffers))))
		     (switch-to-buffer (unless (eq buf (current-buffer)) buf)))))

    (keymap-set! ctl-x-x-map
		 "f" 'font-lock-mode)

    (keymap-set! help-map
		 "."
		 (defun describe-symbol-at-point ()
                   (interactive)
                   (describe-symbol (symbol-at-point))))

    (keymap-set! indent-rigidly-map
		 "C-i" 'indent-rigidly-right-to-tab-stop
		 "C-M-i" 'indent-rigidly-left-to-tab-stop
		 "SPC" 'indent-rigidly-right
		 "DEL" 'indent-rigidly-left)))

(use-package hippie-exp
  :config
  (keymap-set! global-map "C-M-/" 'hippie-expand)

  (advice-add 'try-expand-list
	      :after
	      (lambda (&rest _)
		(when electric-pair-mode
		  (backward-delete-char 1)))))

(use-package window
  :config
  (setopt switch-to-buffer-obey-display-actions t
	  display-buffer-alist
	  `((,(rx "*"
		  (or "Completions"
		      "Register Preview"
		      "Agenda Commands"
		      "transient"
		      (seq "Org " (or "Help" "todo" "Select")))
		  "*")
	     display-buffer-at-bottom)
	    ("." (display-buffer-same-window
		  display-buffer-use-some-window))))

  (advice-add 'switch-to-buffer-other-window :override 'switch-to-buffer)

  (with-eval-after-load 'ediff
    (setopt ediff-window-setup-function 'ediff-setup-windows-plain))

  (with-eval-after-load 'man
    (setopt Man-notify-method 'pushy))

  (with-eval-after-load 'org
    (setopt org-src-window-setup 'current-window
	    org-agenda-window-setup 'current-window
	    org-use-fast-todo-selection 'expert)))

(progn
  (setopt line-spacing (/ 1.0 5))
  (add-to-list 'default-frame-alist '(font . "Aporetic Sans Mono 10")))

(use-package modus-themes
  :init
  (setopt modus-themes-italic-constructs t
	  modus-themes-bold-constructs t
	  modus-themes-common-palette-overrides '((fg-region unspecified)
						  (fg-heading-1 fg-heading-0)
						  (bg-prose-block-contents bg-dim)))
  (with-eval-after-load 'comint
    (set-face-bold 'comint-highlight-prompt t))
  (load-theme 'modus-operandi))

(use-package register
  :config
  (setopt register-use-preview 'never)

  (defun buffer-to-register (reg buffer)
    (interactive (list (register-read-with-preview "Buffer to register: ")
		       (current-buffer)))
    (set-register reg `(buffer . ,buffer)))

  (progn
    (buffer-to-register ?O "*Occur*")
    (buffer-to-register ?T "*scratch*")
    (buffer-to-register ?C "*compilation*")
    (buffer-to-register ?H "*Help*")
    (set-register ?I `(file . ,user-init-file)))

  (defun point-to-register-dwim ()
    (interactive)
    (call-interactively
     (if current-prefix-arg
         'buffer-to-register
       'point-to-register)))

  (keymap-set! global-map
               "<remap> <point-to-register>" 'point-to-register-dwim
	       "M-r" 'point-to-register-dwim
	       "M-j" 'jump-to-register)

  (keymap-set! global-map
	       "<remap> <copy-to-register>"
	       (defun copy-to-register-dwim (&optional arg)
		 (interactive "P")
		 (if (use-region-p)
		     (call-interactively 'copy-to-register)
		   (let ((text (read-from-kill-ring "Text: "))
			 (reg (register-read-with-preview "Copy to register: ")))
		     (set-register reg text))))))

(use-package ffap
  :config
  (ffap-bindings))

(use-package hl-line
  :config
  (global-hl-line-mode))

(progn
  (global-font-lock-mode -1)

  (with-eval-after-load 'diff-mode
    (add-hook 'diff-mode-hook 'font-lock-mode))

  (with-eval-after-load 'magit
    (add-hook 'magit-mode-hook 'font-lock-mode)))

(use-package comint
  :config
  (setopt comint-input-ignoredups t
	  comint-prompt-read-only t))

(use-package isearch
  :config
  (add-hook 'isearch-update-post-hook
            (lambda ()
	      (when (and isearch-other-end
                         isearch-forward
                         (string-prefix-p "isearch" (symbol-name last-command)))
                (goto-char isearch-other-end))))

  (setopt isearch-lax-whitespace t
          search-whitespace-regexp ".*?")

  (setopt lazy-highlight-initial-delay 0
          isearch-lazy-count t
          isearch-allow-motion t
          isearch-motion-changes-direction t
          isearch-wrap-pause 'no)

  (progn
    (keymap-unset isearch-mode-map "C-M-d" t)
    (keymap-set! isearch-mode-map
		 "<remap> <isearch-delete-char>" 'isearch-del-char)))

(use-package repeat
  :config
  (repeat-mode)
  (setopt set-mark-command-repeat-pop t)

  (progn
    (put 'first-error 'repeat-map 'next-error-repeat-map)
    (keymap-set! next-error-repeat-map "M-<" 'first-error)
    (keymap-set! goto-map "M-<" 'first-error))

  (progn
    (setopt repeat-keep-prefix t)

    (defmacro set-repeat! (&rest pairs)
      (macroexp-progn
       (cl-loop for (key cmd)
		on pairs
		by 'cddr
		collect `(defvar-keymap ,(intern (format "%s-repeat-map" (eval cmd)))
			   :repeat t
			   ,key ,cmd))))

    (set-repeat! "M-t" 'transpose-words
		 "C-t" 'transpose-chars
		 "C-M-t" 'transpose-sexps
		 "C-t" 'transpose-lines
		 "M-h" 'mark-end-of-sentence
		 "M-k" 'kill-sentence
		 "M-^" 'delete-indentation
		 "C-;" 'comment-line
		 "C-o" 'delete-blank-lines
		 "k" 'kill-current-buffer))

  (with-eval-after-load 'org
    (defvar-keymap org-heading-repeat-map
      :repeat t
      "C-n" 'org-next-visible-heading
      "C-p" 'org-previous-visible-heading
      "C-b" 'org-backward-heading-same-level
      "C-f" 'org-forward-heading-same-level
      "C-u" 'outline-up-heading
      "C-^" 'org-up-element
      "C-_" 'org-down-element)))

(use-package replace
  :config
  (progn
    (defvar self-insert-ignored-map
      (let ((map (make-keymap)))
	(set-char-table-range (nth 1 map) t 'ignore)
	map))

    (define-keymap :keymap query-replace-map :parent self-insert-ignored-map))

  (keymap-set! global-map
	       "<remap> <query-replace>" 'query-replace-regexp
	       "<remap> <isearch-query-replace>" 'isearch-query-replace-regexp)

  (keymap-set! query-replace-map
	       "p" 'backup))

(use-package icomplete
  :config
  (fido-vertical-mode)

  (setopt completion-auto-help nil)

  (setopt icomplete-delay-completions-threshold 0
          icomplete-compute-delay 0
          icomplete-tidy-shadowed-file-names t
          completions-detailed t
	  icomplete-prospects-height 12
	  max-mini-window-height 12)

  (setopt completion-ignore-case t
          read-buffer-completion-ignore-case t
          read-file-name-completion-ignore-case t)

  (keymap-set! icomplete-minibuffer-map
	       "C-i" 'icomplete-force-complete
	       "C-M-m" 'icomplete-fido-exit

	       "M-w"
	       (defun icomplete-save (&optional arg)
		 (interactive "P")
		 (if (use-region-p)
		     (call-interactively 'kill-ring-save)
		   (kill-new (substring-no-properties
			      (if arg
				  (minibuffer-contents)
				(car completion-all-sorted-completions))))))))

(use-package bookmark
  :config
  (setopt bookmark-fringe-mark nil
          bookmark-save-flag 1))

(use-package savehist
  :config
  (savehist-mode)
  (setopt savehist-additional-variables
	  '(kill-ring
	    register-alist
	    mark-ring
	    global-mark-ring
	    search-ring
	    regexp-search-ring)))

(use-package dired
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'dired-omit-mode)

  (setopt dired-kill-when-opening-new-dired-buffer t
	  dired-free-space nil
          dired-omit-files "^\\..*$"
          dired-clean-confirm-killing-deleted-buffers nil
          dired-dwim-target t
          dired-listing-switches "-alhF"
          dired-recursive-copies 'always
          dired-recursive-deletes 'always
          dired-no-confirm '(uncompress move copy)
          dired-create-destination-dirs 'ask
          dired-vc-rename-file t
          dired-auto-revert-buffer 'dired-directory-changed-p
          dired-create-destination-dirs-on-trailing-dirsep t)

  (keymap-set! dired-mode-map
	       "b" 'dired-up-directory)

  (progn
    (defvar dired-archive-dir "~/Archive/")

    (keymap-set! dired-mode-map
		 "C-c a"
		 (defun dired-archive ()
		   (interactive)
		   (seq-do (lambda (file)
			     (let* ((dest (file-name-concat
					   dired-archive-dir
					   (concat
					    (file-name-sans-extension (file-relative-name file "~/"))
					    "_archived_"
					    (format-time-string "%F_T%H-%M-%S")
					    (when (file-name-extension file)
					      (concat "." (file-name-extension file))))))
				    (dir (file-name-directory dest)))
			       (unless (file-exists-p dir)
				 (make-directory dir t))
			       (rename-file file dest 1)))
			   (dired-get-marked-files nil nil))
		   (revert-buffer)))))

(progn
  (use-package org
    :config
    (progn
      (add-hook 'org-mode-hook 'org-indent-mode)
      (setopt org-indent-indentation-per-level standard-indent))

    (setopt org-tags-column 0
	    org-auto-align-tags nil
	    org-reverse-note-order t
            org-use-property-inheritance t
	    org-fontify-quote-and-verse-blocks t
	    org-hide-emphasis-markers t
	    org-pretty-entities t
	    org-startup-folded 'nofold
	    org-list-demote-modify-bullet
	    '(("-" . "-")
	      ("1." . "1.")))

    (setopt org-priority-highest 1
	    org-priority-lowest 3)

    (setopt org-outline-path-complete-in-steps nil
	    org-goto-interface 'outline-path-completion)

    (setopt org-refile-use-outline-path t
	    org-refile-targets `((nil . (:maxlevel . 8))))

    (progn
      (setopt org-todo-keywords `((sequence
				   "[ ](t)"
				   "[?](?)"
				   "[+](+)"
				   "[-](-)"
				   "[@](2)"
				   "[=](=)"
				   "[&](&)"
				   "|"
				   "[X](x!)"
				   "[\\](\\!)")))

      (setopt org-log-into-drawer "LOG"
	      org-log-note-headings
	      '((done		.	"%t :: %s")
		(state		.	"%t :: %s")
		(note		.	"%t")
		(reschedule	.	"%t :: reschedule %s")
		(delschedule	.	"%t :: delschedule")
		(redeadline	.	"%t :: redeadline %s")
		(deldeadline	.	"%t :: deldeadline")
		(refile		.	"%t :: refile")
		(clock-out	.	""))))

    (modify-syntax-entry ?: "_" org-mode-syntax-table)

    (setopt org-special-ctrl-k t)

    (keymap-set! ctl-x-map
		 "t" 'org-capture)

    (keymap-set! org-mode-map
		 "<remap> <org-transpose-element>" 'transpose-sexps

		 "M-}" 'org-forward-paragraph
		 "M-{" 'org-backward-paragraph
		 "M-n" 'org-forward-element
		 "M-p" 'org-backward-element

		 "M-m"
		 (defun org-back-to-indentation ()
		   (interactive)
		   (let ((org-special-ctrl-a/e t))
		     (call-interactively 'org-beginning-of-line))))

    (keymap-set! org-src-mode-map
		 "C-c C-c" 'org-edit-src-exit)

    (with-eval-after-load 'puni
      (keymap-set! org-mode-map
		   "<remap> <puni-kill-line>"
		   (defun puni-org-kill-line-dwim ()
		     (interactive)
		     (call-interactively
		      (if (puni-beginning-pos-of-sexp-around-point)
			  'puni-kill-line
			'org-kill-line))))))

  (use-package org-reverse-datetree
    :ensure t
    :after org)

  (use-package doct
    :ensure t
    :after (org org-reverse-datetree)
    :config
    (setopt
     org-capture-templates
     (doct
      (let* ((todo (lambda (&rest properties)
		     (append `("todo" :keys "t")
			     properties
			     `(:headline
			       "agenda"
			       :template
			       ("* [ ] %^{title}"
				":PROPERTIES:"
				":date: %u"
				":END:")))))

	     (event (lambda (&rest properties)
		      (append `("event" :keys "e")
			      properties
			      `(:headline
				"events"
				:template
				("* %^{title}"
				 "SCHEDULED: %^t"
				 ":PROPERTIES:"
				 ":date: %u"
				 ":END:"
				 "- location :: %^{location}"
				 "- material :: %^{material}")))))

	     (note (lambda (&rest properties)
		     (append `("note" :keys "n")
			     properties
			     `(:headline
			       "notes"
			       :template
			       ("* %^{title} %^g"
				":PROPERTIES:"
				":date: %u"
				":END:"
				"%?")))))

	     (agenda-file-name "agenda.org"))

	`((:group
	   "all"
	   :prepend t
	   :empty-lines-before 1
	   :children
	   (("personal" :keys "p"
	     :file ,(file-name-concat "~/Documents/personal/" agenda-file-name)
	     :children ,(seq-map 'funcall (list todo note event)))

	    ("wiki" :keys "w"
	     :file ,(file-name-concat "~/Documents/wiki/" agenda-file-name)
	     :children ,(seq-map 'funcall (list todo note)))

	    ,(let ((directory "~/Documents/uni/cs/s4/"))
	       `("uni" :keys "u"
		 :file ,(file-name-concat directory agenda-file-name)
		 :children
		 (,@(seq-map 'funcall (list todo event))

		  ,@(seq-map (lambda (name)
			       `(,name :keys ,(downcase (substring name 0 1))
				       :file ,(file-name-concat directory name agenda-file-name)
				       :children ,(seq-map 'funcall (list todo note))))
			     '("FMFP" "PS" "DMDB" "CN")))))

	    ("journal" :keys "j"
	     :function (lambda () (org-reverse-datetree-2 nil '("[%Y-%m-%d %A]")))
	     :file "~/Documents/personal/journal/journal.org"
	     :children (,(funcall todo :headline nil :unnarrowed t)
			,(funcall note :headline nil :prepend nil)))

	    ,(let* ((file "~/Documents/literature/literature.org"))
	       `("literature" :keys "l"
		 :file ,file
		 :children
		 (,(funcall todo)

		  ("add source" :keys "a"
		   :headline "sources"
		   :template
		   ("* [ ] %^{title} %^g"
		    ":PROPERTIES:"
		    ":date: %u"
		    ":title: %\\1"
		    ":subtitle: %^{subtitle}"
		    ":author: %^{author}"
		    ":year: %^{year}"
		    ":type: %^{type|novel|textbook|scholary_article|article|guide}"
		    ":pages: %^{pages}"
		    ":END:"))

		  (:group
		   "notes"
		   :function (lambda ()
			       (let ((org-refile-use-outline-path nil)
				     (org-refile-targets '(((,file) . (:todo . "[-]")))))
				 (org-refile t nil nil "Source: ")))
		   :prepend nil
		   :children
		   (("quote" :keys "o"
		     :template
		     ("* %^{title} :quote:%^g"
		      ":PROPERTIES:"
		      ":date: %u"
		      ":page: %^{page}"
		      ":END:"
		      "#+begin_quote"
		      "%?"
		      "#+end_quote"))

		    ,(funcall note :headline nil))))))))))))))

(use-package puni
  :ensure t
  :init
  (puni-global-mode)
  (with-eval-after-load 'pdf-view
    (add-hook 'pdf-view-mode-hook (lambda () (puni-mode -1))))

  (setopt puni-blink-for-sexp-manipulating t)

  (advice-add 'puni-kill-line
	      :around
	      (lambda (fn &rest args)
		(save-excursion
		  (apply fn args))))

  (progn
    (keymap-set! puni-mode-map
                 "C-M-r" 'puni-raise
                 "C-M-s" 'puni-splice

                 "C-(" 'puni-slurp-backward
                 "C-)" 'puni-slurp-forward
                 "C-{" 'puni-barf-backward
                 "C-}" 'puni-barf-forward

		 "C-<backspace>"
		 (defun puni-backward-kill-line-to-indent (&optional arg)
		   (interactive "P")
		   (let ((indent (save-excursion (back-to-indentation) (point))))
		     (if (or arg (<= (point) indent))
			 (puni-backward-kill-line arg)
		       (puni-soft-delete (point) indent 'strict-sexp 'beyond 'kill))))))

  (keymap-set! lisp-mode-shared-map
               "C-c v" 'puni-convolute
	       "C-c s" 'puni-split))

(use-package magit
  :ensure t)

(use-package project
  :defer t
  :config
  (setopt project-switch-commands 'project-find-file
	  project-mode-line t))

(use-package tramp
  :defer t
  :config
  (setopt tramp-auto-save-directory (file-name-concat user-emacs-directory
						      "tramp-autosave/")))

(use-package whisper
  :ensure t
  :defer t
  :vc (:url "https://github.com/natrys/whisper.el")
  :config
  (add-hook 'whisper-after-transcription-hook
	    (lambda ()
	      (downcase-region (point-min) (point-max))
	      (repunctuate-sentences t (point-min) (point-max)))))

(use-package pdf-tools
  :ensure t
  :defer t
  :init (pdf-tools-install)
  :config
  (setopt pdf-view-continuous nil
	  large-file-warning-threshold (expt 10 8)
	  pdf-view-display-size 'fit-height
	  pdf-view-resize-factor 1.1))

(progn
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (use-package nov
    :ensure t
    :defer t
    :config
    (setopt nov-variable-pitch nil)))
