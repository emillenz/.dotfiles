;; -*- lexical-binding: t -*-
;; ---
;; title: minimalist, distractionless vanilla emacs config
;; date: [2025-05-09]
;; author: emil lenz
;; email: emillenz@protonmail.com
;; ---

(setopt use-package-always-demand t
	use-package-enable-imenu-support t)

(use-package emacs
  :config
  (global-auto-revert-mode)
  (column-number-mode)
  (global-word-wrap-whitespace-mode)
  (global-subword-mode)
  (delete-selection-mode)
  (electric-indent-mode)
  (electric-pair-mode)
  (repeat-mode)
  (save-place-mode)
  (auto-revert-mode)
  (global-visual-line-mode)
  (auto-save-visited-mode)

  (fringe-mode 0)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (scroll-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (tooltip-mode -1)

  (setopt uniquify-buffer-name-style 'forward
	  delete-by-moving-to-trash t
	  remote-file-name-inhibit-delete-by-moving-to-trash t
	  ring-bell-function 'ignore
	  enable-recursive-minibuffers t
	  global-auto-revert-non-file-buffers t
	  auto-save-include-big-deletions t
	  kill-buffer-delete-auto-save-files t
	  auto-save-list-file-prefix (expand-file-name "autosave/" user-emacs-directory)
	  custom-file (expand-file-name "custom.el" user-emacs-directory)
	  use-short-answers t
	  save-interprogram-paste-before-kill t
	  require-final-newline t
	  load-prefer-newer t
	  desktop-dirname user-emacs-directory
	  find-file-visit-truename t
	  comment-empty-lines nil
	  register-preview-delay nil
	  show-paren-when-point-inside-paren t
	  kill-do-not-save-duplicates t
	  kill-whole-line t
	  shift-select-mode nil
	  kmacro-execute-before-append nil)

  (setopt history-length 1000
	  history-delete-duplicates t)

  (setopt shell-command-prompt-show-cwd t
	  async-shell-command-display-buffer nil
	  async-shell-command-buffer 'new-buffer)

  (setopt bookmark-fringe-mark nil
	  bookmark-save-flag 1)

  (setopt create-lockfiles nil
	  make-backup-files nil
	  delete-old-versions t
	  version-control t
	  kept-new-versions 5
	  kept-old-versions 5
	  vc-follow-symlinks t
	  vc-make-backup-files nil
	  backup-by-copying t
	  backup-by-copying-when-linked t
	  backup-directory-alist (list (cons "." (concat user-emacs-directory "backups"))))

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

  (setopt tab-always-indent 'complete
	  indent-tabs-mode t
	  tab-width 8
	  standard-indent 8
	  c-default-style "linux")

  (setopt compilation-scroll-output t
	  next-error-recenter '(4))

  (setopt scroll-preserve-screen-position t
	  scroll-error-top-bottom t
	  auto-window-vscroll nil)

  (setopt comint-input-ignoredups t
	  comint-prompt-read-only t)

  (progn
    (global-hl-line-mode)
    (add-hook 'deactivate-mark-hook (lambda () (global-hl-line-mode 1)))
    (add-hook 'activate-mark-hook (lambda () (global-hl-line-mode -1))))

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (delete 'try-expand-list hippie-expand-try-functions-list)

  (add-to-list 'default-frame-alist '(font . "iosevka comfy-10"))

  (progn
    (setopt minibuffer-prompt-properties
	    '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode))

  (progn
    (set-language-environment "utf-8")
    (setopt default-input-method nil))

  (progn
    (require-theme 'modus-themes)
    (setopt modus-themes-italic-constructs t
	    modus-themes-bold-constructs t
	    modus-themes-common-palette-overrides '((fg-heading-1 fg-heading-0)
						    (bg-prose-block-contents bg-dim)))
    (load-theme 'modus-operandi)

    (progn
      (global-font-lock-mode -1)
      (with-eval-after-load 'magit
	(add-hook 'magit-mode-hook 'font-lock-mode))))

  (progn
    (modify-syntax-entry ?. "_" (standard-syntax-table))
    (modify-syntax-entry ?: "_" (standard-syntax-table)))

  (put 'narrow-to-region 'disabled nil)

  (seq-do (lambda (fn)
	    (advice-add fn :before (lambda (&rest _) (deactivate-mark))))
	  '(apply-macro-to-region-lines
	    eval-region
	    align
	    align-entire))

  (progn
    (defun push-mark-once (fn)
      (advice-add fn
		  :before
		  (lambda (&rest _)
		    (unless (or (eq last-command this-command)
				(when (mark) (= (mark) (point))))
		      (push-mark)))))

    (seq-do 'push-mark-once '(mark-paragraph
			      backward-up-list))

    (add-hook 'deactivate-mark-hook
	      (lambda ()
		(unless (eq (mark) (point))
		  (pop-mark)))))

  (seq-do (lambda (cmd)
	    (advice-add cmd :around
			(lambda (fn &rest args)
			  (with-undo-amalgamate
			    (apply fn args)))))
	  '(kmacro-end-and-call-macro
	    query-replace-regexp
	    apply-macro-to-region-lines))

  (progn
    (defmacro keymap-set! (keymap &rest pairs)
      (macroexp-progn
       (cl-loop for (key cmd)
		on pairs
		by 'cddr
		collect (list 'keymap-set keymap key cmd))))

    (keymap-set! global-map
		 "<remap> <downcase-word>" 'downcase-dwim
		 "<remap> <upcase-word>" 'upcase-dwim
		 "<remap> <capitalize-word>" 'capitalize-dwim
		 "<remap> <dabbrev-expand>" 'hippie-expand
		 "<remap> <kill-buffer>" 'kill-buffer-and-window
		 "<remap> <list-buffers>" 'ibuffer
		 "<remap> <dired>" 'dired-jump

		 "C-," (lambda () (interactive) (set-mark-command t))
		 "C-z" 'repeat

		 "<remap> <delete-horizontal-space>" 'cycle-spacing
		 "M-SPC" 'mark-word)

    (keymap-set! ctl-x-map
		 "f" 'recentf-open)

    (keymap-set! ctl-x-x-map
		 "f" 'global-font-lock-mode)

    (keymap-set! indent-rigidly-map
		 "C-i" 'indent-rigidly-right-to-tab-stop
		 "C-M-i" 'indent-rigidly-left-to-tab-stop
		 "SPC" 'indent-rigidly-right
		 "DEL" 'indent-rigidly-left)

    (keymap-set! next-error-repeat-map
		 "<" 'first-error)

    (progn
      (setopt kill-read-only-ok t)
      (defun kill-ring-save-region-or-next-kill ()
	(interactive)
	(if (use-region-p)
	    (call-interactively 'kill-ring-save)
	  (let ((buffer-read-only t))
	    (save-mark-and-excursion
	      (call-interactively
	       (key-binding
		(read-key-sequence "save next kill:")))))))

      (keymap-set! global-map "M-w" 'kill-ring-save-region-or-next-kill))

    (progn
      (defun open-line-indent ()
	(interactive)
	(save-mark-and-excursion
	  (seq-do 'call-interactively '(open-line forward-line))
	  (indent-according-to-mode)))

      (keymap-set! global-map "<remap> <open-line>" 'open-line-indent))

    (progn
      (defun yank-indent ()
	(interactive)
	(let ((pos (point)))
	  (if (and delete-selection-mode (use-region-p))
	      (call-interactively 'delete-region))
	  (call-interactively 'yank)
	  (indent-region pos (point))))

      (keymap-set! global-map "<remap> <yank>" 'yank-indent))

    (progn
      (defun comment-sexp-dwim ()
	(interactive)
	(save-mark-and-excursion
	  (unless (use-region-p)
	    (call-interactively 'mark-sexp))
	  (call-interactively 'comment-dwim)))

      (keymap-set! global-map "<remap> <comment-dwim>" 'comment-sexp-dwim))

    (progn
      (defun eval-sexp-dwim ()
	(interactive)
	(save-mark-and-excursion
	  (unless (use-region-p)
	    (call-interactively 'mark-sexp))
	  (eval-region (region-beginning) (region-end) t)))

      (defun pp-eval-sexp-dwim ()
	(interactive)
	(save-mark-and-excursion
	  (unless (use-region-p)
	    (call-interactively 'mark-sexp))
	  (pp-eval-expression (read (buffer-substring (region-beginning)
						      (region-end))))))

      (keymap-set! global-map "<remap> <eval-last-sexp>" 'eval-sexp-dwim))

    (progn
      (defun switch-to-other-buffer ()
	(interactive)
	(let ((buf (caar (window-prev-buffers))))
	  (switch-to-buffer (unless (eq buf (current-buffer)) buf))))

      (keymap-set! global-map "M-o" 'switch-to-other-buffer))

    (progn
      (defun buffer-to-register (reg)
	(interactive (list (register-read-with-preview "buffer to register:")))
	(set-register reg (cons 'buffer (current-buffer))))

      (defun point-to-register-dwim ()
	(interactive)
	(call-interactively
	 (if current-prefix-arg
	     'buffer-to-register
	   'point-to-register)))

      (keymap-set! global-map
		   "M-r" 'point-to-register-dwim
		   "M-j" 'jump-to-register))

    (progn
      (defun kmacro-end-and-call-macro-dwim ()
	(interactive)
	(call-interactively (if (use-region-p)
				'apply-macro-to-region-lines
			      'kmacro-end-and-call-macro)))
      (keymap-set! global-map
		   "<remap> <kmacro-end-and-call-macro>" 'kmacro-end-and-call-macro-dwim))))

(use-package isearch
  :config
  (add-hook 'isearch-update-post-hook
	    (lambda ()
	      (when (and isearch-other-end
			 isearch-forward
			 (string-prefix-p "isearch" (symbol-name last-command)))
		(goto-char isearch-other-end))))

  (setopt isearch-lax-whitespace t
	  search-whitespace-regexp ".*?"

	  lazy-highlight-initial-delay 0
	  isearch-lazy-count t
	  isearch-allow-motion t
	  isearch-motion-changes-direction t)

  (keymap-unset isearch-mode-map "C-w" t)
  (keymap-unset isearch-mode-map "C-M-d" t)
  (keymap-set! global-map
	       "<remap> <isearch-delete-char>" 'isearch-del-char))

(use-package replace
  :config
  (progn
    (defvar suppressed-map
      (let ((map (make-keymap)))
	(set-char-table-range (nth 1 map) t 'ignore)
	map))
    (define-keymap :keymap query-replace-map :parent suppressed-map))

  (keymap-set! global-map
	       "<remap> <query-replace>" 'query-replace-regexp
	       "<remap> <isearch-query-replace>" 'isearch-query-replace-regexp)

  (keymap-set! query-replace-map
	       "p" 'backup))

(use-package icomplete
  :config
  (icomplete-vertical-mode)

  (setopt completion-auto-help nil)

  (setopt icomplete-delay-completions-threshold 0
	  icomplete-show-matches-on-no-input t
	  icomplete-compute-delay 0
	  icomplete-prospects-height 10
	  icomplete-tidy-shadowed-file-names t
	  icomplete-scroll t
	  completions-detailed t
	  completion-styles '(basic flex))

  (setopt completion-ignore-case t
	  read-buffer-completion-ignore-case t
	  read-file-name-completion-ignore-case t)

  (keymap-set! icomplete-minibuffer-map
	       "C-n" 'icomplete-forward-completions
	       "C-p" 'icomplete-backward-completions
	       "<remap> <minibuffer-complete>" 'icomplete-force-complete
	       "C-m" 'icomplete-force-complete-and-exit
	       "C-M-m" 'icomplete-ret))

(use-package recentf
  :config
  (recentf-mode 1)
  (setopt recentf-max-saved-items 300))

(use-package savehist
  :config
  (savehist-mode 1)
  (setopt savehist-additional-variables
	  '(kill-ring
	    register-alist
	    mark-ring global-mark-ring
	    search-ring regexp-search-ring)))

(use-package dired
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'dired-omit-mode)

  (setopt dired-free-space nil
	  dired-omit-files "^\\..*$"
	  dired-clean-confirm-killing-deleted-buffers nil
	  dired-dwim-target t
	  dired-recursive-copies 'always
	  dired-recursive-deletes 'always
	  dired-no-confirm '(uncompress move copy)
	  dired-create-destination-dirs 'ask
	  dired-vc-rename-file t)

  (keymap-set! dired-mode-map
	       "b" 'dired-up-directory
	       "<remap> <dired-hide-details-mode>" (lambda ()
						     (interactive)
						     (dired-omit-mode 'toggle)
						     (dired-hide-details-mode 'toggle))))

(use-package org
  :config
  (setopt org-tags-column 0))

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

(use-package puni
  :ensure t
  :init (puni-global-mode 1)
  :config
  (seq-do 'push-mark-once '(puni-mark-list-around-point
			    puni-end-of-sexp
			    puni-beginning-of-sexp))

  (advice-add 'puni-kill-line
	      :around
	      (lambda (fn &rest args)
		(if (bolp)
		    (save-excursion
		      (apply fn args))
		  (apply fn args))))

  (defun puni-backward-kill-to-indent ()
    (interactive)
    (let ((indent-begin (save-excursion (back-to-indentation) (point))))
      (if (= (point) indent-begin)
	  (call-interactively 'puni-backward-kill-line)
	(puni-soft-delete (point)
			  indent-begin
			  'strict-sexp
			  'beyond
			  'kill))))

  (keymap-set! global-map
	       "C-<backspace>" 'puni-backward-kill-to-indent

	       "C-M-r" 'puni-raise
	       "C-M-s" 'puni-splice
	       "C-M-v" 'puni-mark-list-around-point

	       "C-(" 'puni-slurp-backward
	       "C-)" 'puni-slurp-forward
	       "C-{" 'puni-barf-backward
	       "C-}" 'puni-barf-forward)

  (keymap-set! lisp-mode-shared-map
	       "C-c v" 'puni-convolute
	       "C-c s" 'puni-split))

(use-package magit
  :ensure t)

(use-package current-window-only
  :ensure t
  :init (current-window-only-mode 1)
  :config
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain)
  (advice-remove 'delete-other-windows
		 'current-window-only--delete-other-windows)
  (add-to-list 'display-buffer-alist
	       (list (rx (seq "*")
			 (or "Completions"
			     "Register Preview")
			 (seq "*"))
		     'display-buffer-at-bottom)))

(use-package pdf-tools
  :ensure t
  :init (pdf-tools-install)
  :config
  (setopt pdf-view-display-size 'fit-height))
