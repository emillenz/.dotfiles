;; ---
;; title: minimalist, distractionless vanilla emacs config
;; date: [2025-05-09]
;; author: emil lenz
;; email: emillenz@protonmail.com
;; ---

;; (setq toggle-debug-on-error t)

(setq use-package-hook-name-suffix nil
      use-package-always-demand t
      use-package-enable-imenu-support t)

(use-package emacs
  :init
  (global-auto-revert-mode)
  (column-number-mode)
  (global-word-wrap-whitespace-mode)
  (global-subword-mode)
  (delete-selection-mode)
  (electric-indent-mode)
  (electric-pair-mode)
  (repeat-mode)
  (fido-vertical-mode)
  (save-place-mode)
  (auto-revert-mode)
  (global-visual-line-mode)
  (global-hl-line-mode)
  (auto-save-visited-mode)
  (fringe-mode 1)

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (scroll-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (tooltip-mode -1)

  (setq initial-scratch-message nil
	inhibit-startup-echo-area-message user-login-name
	initial-buffer-choice nil
	inhibit-splash-screen t
	inhibit-startup-screen t
	inhibit-startup-buffer-menu t
	frame-title-format "%b ― emacs"
	icon-title-format frame-title-format
	frame-inhibit-implied-resize t
	use-file-dialog nil
	use-dialog-box nil

	history-length 1000
	history-delete-duplicates t
	uniquify-buffer-name-style 'forward
	delete-by-moving-to-trash t
	remote-file-name-inhibit-delete-by-moving-to-trash t
	ring-bell-function 'ignore
	enable-recursive-minibuffers t
	global-auto-revert-non-file-buffers t
	auto-save-include-big-deletions t
	kill-buffer-delete-auto-save-files t
	auto-save-list-file-prefix (expand-file-name "autosave/" user-emacs-directory)
	use-short-answers t
	save-interprogram-paste-before-kill t
	require-final-newline t
	load-prefer-newer t
	shell-command-prompt-show-cwd t
	custom-file (expand-file-name "custom.el" user-emacs-directory)
	desktop-dirname user-emacs-directory
	find-file-visit-truename t
	comment-empty-lines nil
	register-preview-delay nil
	kill-do-not-save-duplicates t
	show-paren-when-point-inside-paren t
	mark-even-if-inactive nil
	kill-whole-line t
	shift-select-mode nil
	set-mark-command-repeat-pop t

	compilation-scroll-output t
	next-error-recenter '(4)

	create-lockfiles nil
	make-backup-files nil
	delete-old-versions t
	version-control t
	kept-new-versions 5
	kept-old-versions 5
	vc-follow-symlinks t
	vc-make-backup-files nil
	backup-by-copying t
	backup-by-copying-when-linked t
	backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))

	scroll-preserve-screen-position t
	scroll-error-top-bottom t
	auto-window-vscroll nil

	comint-input-ignoredups t
	comint-prompt-read-only t)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (add-to-list 'default-frame-alist '(font . "iosevka comfy-10"))

  (put 'narrow-to-page 'disabled nil)

  (progn
    (setq minibuffer-prompt-properties
	  '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode))

  (progn
    (set-language-environment "utf-8")
    (setq default-input-method nil))

  (progn
    (require-theme 'modus-themes)
    (setq modus-themes-italic-constructs t
	  modus-themes-bold-constructs t
	  modus-themes-common-palette-overrides '((fg-heading-1 fg-heading-0)
						  (bg-prose-block-contents bg-dim)))
    (load-theme 'modus-operandi)
    (global-font-lock-mode -1))

  (progn
    (setq-default tab-always-indent 'complete
		  indent-tabs-mode t
		  tab-width 8
		  standard-indent 8)
    (setq c-default-style "linux"))

  (progn
    (modify-syntax-entry ?. "_" (standard-syntax-table))
    (modify-syntax-entry ?: "_" (standard-syntax-table)))

  (mapc (lambda (fn)
	  (advice-add fn
		      :after
		      (lambda (&rest r)
			(deactivate-mark))))
	'(apply-macro-to-region-lines
	  eval-region
	  align
	  align-entire))

  (progn
    (advice-add 'mark-paragraph
		:around
		(lambda (fn &rest args)
		  (unless (region-active-p)
		    (push-mark))
		  (apply fn args)))

    (advice-add 'backward-up-list
		:around
		(lambda (fn &rest args)
		  (unless (eq last-command 'backward-up-list)
		    (push-mark))
		  (apply fn args)))

    (add-hook 'deactivate-mark-hook 'pop-mark))

  (defun kill-ring-save-region-or-next-kill ()
    (interactive)
    (if (region-active-p)
	(call-interactively 'kill-ring-save)
      (let ((buffer-read-only t))
	(ignore-errors
	  (save-mark-and-excursion
	    (call-interactively
	     (key-binding
	      (read-key-sequence "save next kill:"))))))))

  (defun set-mark-or-mark-line (n)
    (interactive "p")
    (if (region-active-p)
	(if (< (point) (mark))
	    (let ((beg (point)))
	      (goto-char (mark))
	      (end-of-line)
	      (forward-char 1)
	      (set-mark (point))
	      (goto-char beg)
	      (beginning-of-line))
	  (let ((end (point)))
	    (goto-char (mark))
	    (beginning-of-line)
	    (set-mark (point))
	    (goto-char end)
	    (end-of-line)
	    (forward-char 1)))
      (call-interactively 'set-mark-command)))

  (defun open-line-indent ()
    (interactive)
    (save-mark-and-excursion
      (mapc 'call-interactively '(open-line forward-line))
      (indent-according-to-mode)))

  (defun indent-dwim ()
    (interactive)
    (call-interactively
     (if (region-active-p)
	 'indent-region
       'indent-for-tab-command)))

  (defun yank-indent ()
    (interactive)
    (let ((pos (point)))
      (if (and delete-selection-mode (region-active-p))
	  (call-interactively 'delete-region))
      (call-interactively 'yank)
      (indent-region pos (point))))

  (defun eval-last-sexp-dwim ()
    (interactive)
    (call-interactively (if (region-active-p)
			    'eval-region
			  'eval-last-sexp)))

  (defun comment-sexp-dwim ()
    (interactive)
    (if (region-active-p)
	(call-interactively 'comment-dwim)
      (save-mark-and-excursion
	(mapc 'call-interactively '(mark-sexp comment-dwim)))))

  (defun kmacro-end-and-call-macro-dwim (n)
    (interactive "p")
    (call-interactively
     (if (region-active-p)
	 'apply-macro-to-region-lines
       'kmacro-end-and-call-macro)))

  (defun switch-to-other-buffer ()
    (interactive)
    (let ((other-buffer (caar (window-prev-buffers))))
      (switch-to-buffer (if (eq other-buffer (current-buffer))
			    nil
			  other-buffer))))

  :bind
  ([remap downcase-word] . downcase-dwim)
  ([remap yank] . yank-indent)
  ([remap indent-for-tab-command] . indent-dwim)
  ([remap upcase-word] . upcase-dwim)
  ([remap capitalize-word] . capitalize-dwim)
  ([remap dabbrev-expand] . hippie-expand)
  ([remap eval-last-sexp] . eval-last-sexp-dwim)
  ([remap comment-dwim] . comment-sexp-dwim)
  ([remap open-line] . open-line-indent)
  ([remap set-mark-command] . set-mark-or-mark-line)
  ([remap kmacro-end-and-call-macro] . kmacro-end-and-call-macro-dwim)
  ([remap zap-to-char] . zap-up-to-char)
  ([remap list-buffers] . ibuffer)
  ([remap kill-buffer] . kill-buffer-and-window)
  ([remap dired] . dired-jump)
  ([remap dired-shell-command] . dired-async-shell-command)
  ([remap shell-command] . async-shell-command)

  ("C-u" . (lambda () (interactive) (set-mark-command 1)))
  ("C-z" . repeat)
  ("M-w" . kill-ring-save-region-or-next-kill)

  ("M-o" . switch-to-other-buffer)
  ("M-'" . jump-to-register)

  (:map ctl-x-map
	("f" . find-file))

  (:map ctl-x-x-map
	("f" . global-font-lock-mode))

  (:repeat-map comint-repeat-map
	       ("M-s" . comint-next-matching-input-from-input)
	       ("M-r" . comint-previous-matching-input-from-input))

  (:repeat-map next-error-repeat-map
	       ("M-<" . first-error))

  (:map indent-rigidly-map
	("C-i" . indent-rigidly-right-to-tab-stop)
	("C-S-i" . indent-rigidly-left-to-tab-stop)
	("SPC" . indent-rigidly-right)
	("DEL" . indent-rigidly-left))

  (:map minibuffer-mode-map
	([remap minibuffer-complete] . icomplete-force-complete)
	([remap minibuffer-choose-completion] . icomplete-fido-exit)))

(use-package recentf
  :init
  (recentf-mode 1)
  (setq recentf-max-saved-items 300))

(use-package savehist
  :init
  (savehist-mode 1)
  (setq savehist-save-minibuffer-history t
	savehist-additional-variables
	'(kill-ring
	  register-alist
	  mark-ring global-mark-ring
	  search-ring regexp-search-ring)))

(use-package dired
  :hook
  ((dired-mode-hook . dired-hide-details-mode)
   (dired-mode-hook . dired-omit-mode))

  :init
  (setq dired-free-space nil
	dired-omit-files "^\\..*$"
	dired-clean-confirm-killing-deleted-buffers nil
	dired-recursive-copies 'always
	dired-recursive-deletes 'always
	dired-no-confirm '(uncompress move copy)
	dired-create-destination-dirs 'ask
	dired-vc-rename-file t)

  :bind
  (:map dired-mode-map
	("b" . dired-up-directory)
	("e" . wdired-change-to-wdired-mode)
	([remap dired-hide-details-mode] . (lambda ()
					     (interactive)
					     (dired-omit-mode 'toggle)
					     (dired-hide-details-mode 'toggle)))))
(use-package isearch
  :hook
  ((isearch-update-post-hook
   . (lambda ()
       (when (and isearch-success
		  isearch-forward
		  ;; HACK :: without this isearch won't exit on non-isearch command
		  (string-prefix-p "isearch" (symbol-name last-command)))
	 (goto-char isearch-other-end)))))

  :init
  (setq search-whitespace-regexp ".*?"
	isearch-lax-whitespace t
	lazy-highlight-initial-delay 0
	isearch-lazy-count t
	isearch-allow-motion t
	isearch-motion-changes-direction t)

  (keymap-unset isearch-mode-map "C-w" t)
  (keymap-unset isearch-mode-map "C-M-d" t)

  :bind
  (([remap isearch-delete-char] . isearch-del-char)))

(use-package org
  :init
  (setq org-tags-column 0))

(use-package icomplete
  :init
  (setq icomplete-compute-delay 0
	completions-detailed t
	completions-auto-help 'visible
	completions-max-height 16
	completions-format 'one-column))

(use-package package
  :init
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

(use-package puni
  :ensure t
  :init (puni-global-mode 1)

  :config
  (advice-add 'puni-mark-list-around-point
	      :around
	      (defun puni-mark-list-around-point--advice (fn &rest args)
		(unless (region-active-p)
		  (push-mark))
		(apply fn args)))

  (advice-add 'puni-kill-line
	      :around
	      (defun puni-kill-line--advice (fn &rest args)
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

  :bind
  (("C-M-r" . puni-raise)
   ("C-M-s" . puni-splice)
   ("C-M-v" . puni-mark-list-around-point)
   ("C-(" . puni-slurp-backward)
   ("C-)" . puni-slurp-forward)
   ("C-{" . puni-barf-backward)
   ("C-}" . puni-barf-forward)

   (:map lisp-mode-shared-map
	 ("C-c v" . puni-convolute)
	 ("C-c s" . puni-split))

   (:map global-map
	 ("C-<backspace>" . puni-backward-kill-to-indent))))

(use-package magit
  :ensure t
  :hook ((magit-mode-hook . font-lock-mode))
  :bind
  ((:map magit-mode-map
	 ("C-<tab>" . alternate-buffer))))

(use-package current-window-only
  :ensure t
  :init (current-window-only-mode 1)
  :config
  (advice-remove 'delete-other-windows 'current-window-only--delete-other-windows)
  (add-to-list 'display-buffer-alist '("*Completions*" display-buffer-at-bottom)))

(use-package pdf-tools
  :ensure t
  :init (pdf-tools-install))
