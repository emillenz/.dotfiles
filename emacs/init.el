;; -*- lexical-binding: t -*-

;; ---
;; title: minimalist, distractionless vanilla emacs config
;; date: [2025-05-09]
;; author: emil lenz
;; email: emillenz@protonmail.com
;; ---

(use-package use-package
  :config
  (setopt use-package-always-demand t
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
  (delete-selection-mode)
  (electric-indent-mode)
  (electric-pair-mode)
  (save-place-mode)
  (auto-save-visited-mode)
  (kill-ring-deindent-mode)
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

  (setopt mouse-autoselect-window t
	  uniquify-buffer-name-style 'forward
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

  (progn
    (defun advice-add-push-mark-once (fn)
      (advice-add fn
                  :before
                  (lambda (&rest _)
                    (unless (or (use-region-p)
                                (eq last-command this-command)
                                (when (mark) (= (mark) (point))))
                      (push-mark)))))

    (seq-do 'advice-add-push-mark-once
	    '(mark-paragraph
	      backward-up-list
	      down-list)))

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
		 "<remap> <downcase-word>" 'downcase-dwim
		 "<remap> <upcase-word>" 'upcase-dwim
		 "<remap> <capitalize-word>" 'capitalize-dwim
		 "<remap> <delete-horizontal-space>" 'cycle-spacing
		 "<remap> <kill-buffer>" 'kill-current-buffer
		 "C-M-/" 'hippie-expand
		 "M-SPC" 'mark-word

		 "<remap> <open-line>"
		 (defun open-line-indent ()
		   (interactive)
		   (if (eq (point) (save-excursion
				     (back-to-indentation)
				     (point)))
		       (call-interactively 'split-line)
		     (save-excursion
		       (call-interactively 'default-indent-new-line))))

		 "<remap> <comment-dwim>"
		 (defun comment-sexp-dwim ()
		   (interactive)
		   (save-mark-and-excursion
		     (unless (use-region-p)
		       (call-interactively 'mark-sexp))
		     (call-interactively 'comment-dwim)))

		 "<remap> <eval-last-sexp>"
		 (defun eval-sexp-dwim (&optional arg)
		   (interactive "P")
		   (if (use-region-p)
		       (progn
			 (eval-region (region-beginning) (region-end) t)
			 (deactivate-mark))
		     (save-excursion
		       (forward-sexp)
		       (eval-last-sexp arg))))

		 "<remap> <kmacro-end-and-call-macro>"
		 (defun kmacro-call-dwim ()
		   (interactive)
		   (if (use-region-p)
		       (progn
			 (call-interactively 'apply-macro-to-region-lines)
			 (deactivate-mark))
		     (call-interactively 'kmacro-end-and-call-macro))))

    (keymap-set! ctl-x-map
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
		 "DEL" 'indent-rigidly-left)

    (progn
      (setopt kill-read-only-ok t)

      (keymap-set! global-map
                   "<remap> <kill-ring-save>"
		   (defun kill-ring-save-dwim ()
		     (interactive)
		     (if (use-region-p)
			 (call-interactively 'kill-ring-save)
		       (let ((buffer-read-only t)
			     (kill-read-only-ok t))
			 (save-excursion
			   (call-interactively
			    (key-binding
			     (read-key-sequence "save next kill: "))))))))

      (advice-add 'kill-region
		  :before
		  (lambda (beg end &optional region)
		    (when buffer-read-only
		      (pulse-momentary-highlight-region beg end)))))

    (advice-add 'yank
		:after
		(lambda (&rest _)
                  (unless (minibufferp)
		    (indent-region (mark) (point)))))))

(use-package window
  :config
  (setopt switch-to-buffer-obey-display-actions t
	  display-buffer-alist
	  `((,(rx "*"
		  (or "Completions"
		      "Register Preview"
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

  (progn
    (defun buffer-to-register (reg)
      (interactive (list (register-read-with-preview "buffer to register: ")))
      (set-register reg `(buffer . ,(current-buffer))))

    (defun point-to-register-dwim ()
      (interactive)
      (call-interactively
       (if current-prefix-arg
           'buffer-to-register
         'point-to-register)))

    (keymap-set! global-map
                 "<remap> <point-to-register>" 'point-to-register-dwim
		 "M-#" 'point-to-register-dwim
		 "M-j" 'jump-to-register

		 "<remap> <copy-to-register>"
		 (defun copy-to-register-dwim (&optional arg)
		   (interactive "P")
		   (if (use-region-p)
		       (call-interactively 'copy-to-register)
		     (let ((text (read-from-kill-ring "text: "))
			   (reg (register-read-with-preview "Copy to register: ")))
		       (set-register reg text)))))))

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
    (keymap-unset isearch-mode-map "C-w" t)
    (keymap-unset isearch-mode-map "C-M-d" t)
    (keymap-set! isearch-mode-map "<remap> <isearch-delete-char>" 'isearch-del-char)))

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

    (defmacro keymap-set-repeatable! (&rest pairs)
      (macroexp-progn
       (cl-loop for (key cmd)
		on pairs
		by 'cddr
		collect `(defvar-keymap ,(intern (format "%s-repeat-map" (eval cmd)))
			   :repeat t
			   ,key
			   ,cmd))))

    (keymap-set-repeatable! "M-t" 'transpose-words
			    "C-t" 'transpose-chars
			    "C-M-t" 'transpose-sexps
			    "C-t" 'transpose-lines
			    "M-^" 'delete-indentation
			    "C-;" 'comment-line
			    "C-o" 'delete-blank-lines
			    "k" 'kill-current-buffer))

  (with-eval-after-load 'org
    (defvar-keymap org-heading-repeat-map
      :repeat t
      "C-n" 'org-next-visible-heading
      "C-p" 'org-previous-visible-heading
      "C-u" 'outline-up-heading
      "C-b" 'org-backward-heading-same-level
      "C-f" 'org-forward-heading-same-level)))

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

	       "C-c y"
	       (defun icomplete-yank (&optional arg)
		 (interactive "P")
		 (kill-new (if arg
			       (minibuffer-contents)
			     (car completion-all-sorted-completions)))
		 (run-at-time 0.01
			      nil
			      'call-interactively
			      'yank)
		 (abort-minibuffers))))

(use-package recentf
  :config
  (recentf-mode)
  (setopt recentf-max-saved-items 300))

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
            mark-ring global-mark-ring
            search-ring regexp-search-ring)))

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
					    ".archived_"
					    (format-time-string "%F_T%H-%M-%S")
					    (when (file-name-extension file)
					      (concat "." (file-name-extension file))))))
				    (dir (file-name-directory dest)))
			       (unless (file-exists-p dir)
				 (make-directory dir t))
			       (rename-file file dest 1)))
			   (dired-get-marked-files nil nil))
		   (revert-buffer)))))

(use-package org
  :config
  (progn
    (add-hook 'org-mode-hook 'org-indent-mode)
    (setopt org-indent-indentation-per-level standard-indent))

  (setopt org-tags-column 0
          org-use-property-inheritance t
	  org-reverse-note-order t
	  org-refile-use-outline-path 'full-file-path
	  org-fontify-quote-and-verse-blocks t
	  org-hide-emphasis-markers t
	  org-pretty-entities t
	  org-startup-folded 'nofold
	  org-list-demote-modify-bullet '(("-" . "-")
					  ("1." . "1.")))

  (setopt org-log-done 'time
	  org-log-redeadline 'time
	  org-log-reschedule 'time
	  org-log-into-drawer "LOG")

  (setopt org-priority-highest 1
	  org-priority-lowest 3)

  (setopt org-outline-path-complete-in-steps nil
	  org-goto-interface 'outline-path-completion)

  (progn
    (setopt org-todo-keywords '((sequence
				 "[ ](t!)"
				 "[?](?!)"
				 "[+](+!)"
				 "[-](-!)"
				 "[@](2!)"
				 "[=](=!)"
				 "[&](&!)"
				 "|"
				 "[X](x!)"
				 "[\\](\\!)")))

    (setopt org-log-note-headings
	    '((done		.	"done :: %t")
	      (state		.	"state :: %t %s")
	      (note		.	"note :: %t")
	      (reschedule	.	"reschedule :: %t %s")
	      (delschedule	.	"delschedule :: %t")
	      (redeadline	.	"redeadline :: %t %s")
	      (deldeadline	.	"deldeadline :: %t")
	      (refile		.	"refile :: %t")
	      (clock-out	.	""))))

  (modify-syntax-entry ?: "_" org-mode-syntax-table)

  (setopt org-special-ctrl-k t)

  (keymap-set! org-mode-map
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
    (add-hook 'org-mode-hook
	      (lambda ()
		(keymap-set! org-mode-map
			     "<remap> <puni-kill-line>"
			     (defun puni-org-kill-line ()
			       (interactive)
			       (call-interactively (if (and puni-mode (not (org-at-heading-p)))
						       'puni-kill-line
						     'org-kill-line))))))))

(use-package project
  :config
  (setopt project-switch-commands 'project-find-file
	  project-mode-line t))

(use-package tramp
  :defer t
  :config
  (setopt tramp-auto-save-directory (file-name-concat user-emacs-directory "tramp-autosave/")))

(use-package puni
  :ensure t
  :init
  (puni-global-mode)
  (with-eval-after-load 'pdf-view
    (add-hook 'pdf-view-mode-hook (lambda () (puni-mode -1))))

  (setopt puni-blink-for-sexp-manipulating t)

  (seq-do 'advice-add-push-mark-once
	  '(puni-end-of-sexp
	    puni-beginning-of-sexp))

  (advice-add 'puni-kill-line
	      :around
	      (lambda (fn &rest args)
		(if (bolp)
		    (save-excursion
		      (apply fn args))
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
		   (let ((pos-indent (save-excursion (back-to-indentation) (point))))
		     (if (or arg (<= (point) pos-indent))
			 (puni-backward-kill-line arg)
		       (puni-soft-delete (point)
					 pos-indent
					 'strict-sexp
					 'beyond
					 'kill))))))

  (keymap-set! lisp-mode-shared-map
               "C-c v" 'puni-convolute
	       "C-c s" 'puni-split))

(use-package magit
  :ensure t)

(use-package whisper
  :ensure t
  :vc (:url "https://github.com/natrys/whisper.el"))

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

(use-package org-reverse-datetree
  :ensure t
  :after org
  :config
  (setopt org-reverse-datetree-level-formats '("[%Y-%m-%d %A]")))

(use-package doct
  :ensure t
  :after (org org-reverse-datetree)
  :config
  (defun doct-todo (&optional no-headline-p)
    (append `("todo" :keys "t")
	    (unless no-headline-p `(:headline "tasks"))
	    `(:template ("* [ ] %^{title}"
			 "SCHEDULED: %^t"))))

  (defun doct-event (&optional no-headline-p)
    (append `("event" :keys "e")
	    (unless no-headline-p `(:headline "events"))
	    `(:unnarrowed t)
	    `(:template ("* %^{title}"
			 "%^t"
			 "- location :: %^{location}"
			 "- material :: %^{material}"))))

  (defun doct-note (&optional no-headline-p)
    (append `("note" :keys "n")
	    (unless no-headline-p `(:headline "notes"))
	    `(:template ("* %^{title} %^g"
			 "%U"
			 "%?"))))

  (defun doct-agenda-file (directory)
    (file-name-concat directory "agenda.org"))

  (setopt
   org-capture-templates
   (doct
    `((:group
       "all"
       :prepend t
       :empty-lines-before 1
       :children
       (("personal" :keys "p"
	 :file ,(doct-agenda-file "~/Documents/personal/")
	 :children ,(list (doct-todo)
			  (doct-note)
			  (doct-event)))

	("wiki" :keys "w"
	 :file ,(doct-agenda-file "~/Documents/wiki/")
	 :children ,(list (doct-todo)
			  (doct-note)))

	("uni" :keys "u"
	 :file ,(doct-agenda-file "~/Documents/uni/cs/s4/")
	 :children
	 ,(let ((directory "~/Documents/uni/S4/"))
	    `(("uni" :keys "u"
	       :file ,(doct-agenda-file directory)
	       :children ,(list (doct-todo)
				(doct-note)
				(doct-event)))

	      ,@(seq-map (lambda (name)
			   `(,name :keys ,(downcase (substring name 0 1))
				   :file ,(doct-agenda-file (file-name-concat directory name))
				   :children ,(list (doct-todo)
						    (doct-note))))
			 '("FMFP" "PS" "DMDB" "CN")))))

	("journal" :keys "j"
         :function org-reverse-datetree-goto-date-in-file
	 :unnarrowed t
	 :prepend nil
	 :file "~/Documents/personal/journal/journal.org"
	 :children ,(list (doct-todo t)
			  (doct-note t)))

	("literature" :keys "l"
	 :children
	 ,(let* ((literature-dir "~/Documents/literature/")
		 (notes-dir (file-name-concat literature-dir "notes/")))

	    `((,@(doct-todo t)
	       :file ,(file-name-concat literature-dir "readlist.org"))

	      ("init source" :keys "i"
	       :file (lambda ()
		       (file-name-concat
			,notes-dir
			(replace-regexp-in-string
			 " "
			 "-"
			 (let ((title (read-from-minibuffer "title: ")))
			   (kill-new title)
			   title))))
	       :type plain
	       :template
	       ("#+title: %^{title}: %^{subtitle}"
		"#+author: %n"
		"#+email: %(message-user-mail-address)"
		"#+date: %<%F>"
		"#+filetags: :literature:%^g"
		""
		"* [ ] %\\1"
		"%U"
		":PROPERTIES:"
		":title: %\\1"
		":subtitle: %\\2"
		":author: %^{author}"
		":year: %^{year}"
		":type: %^{type|book|textbook|paper|article|audiobook|podcast}"
		":pages: %^{pages}"
		":END:"))

	      (:group
	       "notes"
	       :file (lambda () (read-file-name "file: " ,notes-dir))
	       :prepend nil
	       :children
	       (("quote" :keys "q"
		 :headline "quotes"
		 :template
		 ("* %^{title} :quote:%^g"
		  "%U"
		  ":PROPERTIES:"
		  ":page: %^{page}"
		  ":END:"
		  "#+begin_quote"
		  "%?"
		  "#+end_quote"))

		,(doct-note))))))))))))
