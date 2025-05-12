;; ---
;; title: minimalist, distractionless vanilla emacs config
;; date: [2025-05-09]
;; author: emil lenz
;; email: emillenz@protonmail.com
;; ---

(setq use-package-hook-name-suffix nil
      use-package-always-demand t
      use-package-enable-imenu-support t)

(use-package emacs
  :init
  (fringe-mode 1)
  (global-auto-revert-mode 1)
  (column-number-mode 1)
  (global-word-wrap-whitespace-mode 1)
  (global-subword-mode 1)
  (delete-selection-mode 1)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (repeat-mode 1)
  (fido-vertical-mode 1)
  (save-place-mode 1)
  (auto-revert-mode 1)
  (global-visual-line-mode 1)

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

	use-file-dialog nil
	use-dialog-box nil
	set-mark-command-repeat-pop t
	history-length 1000
	uniquify-buffer-name-style 'forward
	delete-by-moving-to-trash t
	remote-file-name-inhibit-delete-by-moving-to-trash t
	ring-bell-function 'ignore
	enable-recursive-minibuffers t
	global-auto-revert-non-file-buffers t
	auto-save-include-big-deletions t
	kill-buffer-delete-auto-save-files t
	auto-save-list-file-prefix (expand-file-name "autosave/"
						     user-emacs-directory)
	use-short-answers t
	save-interprogram-paste-before-kill t
	require-final-newline t
	load-prefer-newer t
	backup-by-copying t
	shell-command-prompt-show-cwd t
	backup-directory-alist `(("." .
				  ,(concat user-emacs-directory "backups")))
	custom-file (expand-file-name "custom.el" user-emacs-directory)
	desktop-dirname user-emacs-directory
	find-file-visit-truename t
	comment-empty-lines nil
	register-preview-delay nil
	icomplete-compute-delay 0
	auto-window-vscroll nil
	kill-do-not-save-duplicates t
	show-paren-when-point-inside-paren t
	kill-whole-line t

	compilation-scroll-output t
	next-error-recenter '(4)

	create-lockfiles nil
	make-backup-files nil

	vc-follow-symlinks t
	vc-make-backup-files nil

	scroll-preserve-screen-position t
	scroll-error-top-bottom t

	lazy-highlight-initial-delay 0
	isearch-lazy-count t
	isearch-allow-motion t
	isearch-motion-changes-direction t

	frame-title-format "%b ― emacs"
	icon-title-format frame-title-format
	frame-inhibit-implied-resize t

	comint-input-ignoredups t
	comint-prompt-read-only t)

  (setq-default comment-column 0)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (add-to-list 'default-frame-alist '(font . "iosevka comfy-10"))

  ;; (progn
  ;;   (setq minibuffer-prompt-properties
  ;; 	  '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  ;;   (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode))

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
    (setq display-buffer-base-action '(display-buffer-same-window
				       display-buffer-use-some-window)
	  display-buffer-alist
	  `((,(rx (seq "*"
		       (or "transient"
			   (seq "org " (or "select" "todo"))
			   "agenda commands"
			   "completions")))
	     display-buffer-at-bottom
	     (window-height . fit-window-to-buffer))
	    ("." ,display-buffer-base-action
	     (reuseable-frames . t))))
    (with-eval-after-load 'ediff
      (setq ediff-window-setup-function 'ediff-setup-windows-plain))
    (with-eval-after-load 'org
      (setq org-src-window-setup 'current-window
	    org-agenda-window-setup 'current-window))
    (with-eval-after-load 'man
      (setq man-notify-method 'pushy))
    (advice-add 'switch-to-buffer-other-window :override 'switch-to-buffer)
    (advice-add 'pop-to-buffer :override 'switch-to-buffer))

  (mapc (lambda (fn)
	  (advice-add fn :after
		      (lambda (&rest r)
			(deactivate-mark))))
	'(apply-macro-to-region-lines
	  eval-region
	  align
	  align-entire))

  (advice-add 'mark-paragraph :around
	      (lambda (fn &rest args)
		(unless (region-active-p)
		  (push-mark))
		(apply fn args)))

  (defun kill-ring-save-region-or-next-kill ()
    (interactive)
    (if (region-active-p)
	(call-interactively 'kill-ring-save)
      (let ((buffer-read-only t))
	(ignore-errors
	  (save-mark-and-excursion
	    (call-interactively (key-binding (read-key-sequence "save next kill:")) ))))))

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

  (defun yank-indent ()
    (interactive)
    (let ((pos (point)))
      (if (and delete-selection-mode (region-active-p))
	  (call-interactively 'delete-region))
      (call-interactively 'yank)
      (indent-region pos (point))))

  (defun indent-region-dwim ()
    (interactive)
    (if (region-active-p)
	(call-interactively 'indent-region)
      (save-mark-and-excursion
	(mapc 'call-interactively '(mark-paragraph indent-region)))))

  (defun eval-last-sexp-dwim ()
    (interactive)
    (call-interactively (if (region-active-p)
			    'eval-region
			  'eval-last-sexp)))

  (defun kmacro-end-or-call-macro-dwim (n)
    (interactive "p")
    (call-interactively
     (if (region-active-p)
	 'apply-macro-to-region-lines
       'kmacro-end-and-call-macro)))

  (defun window-alternate-buffer ()
    (interactive)
    (switch-to-buffer (caar (window-prev-buffers))))

  :bind
  (([remap downcase-word] . downcase-dwim)
   ([remap yank] . yank-indent)
   ([remap upcase-word] . upcase-dwim)
   ([remap capitalize-word] . capitalize-dwim)
   ([remap dabbrev-completion] . hippie-expand)
   ([remap list-buffers] . ibuffer)
   ([remap eval-last-sexp] . eval-last-sexp-dwim)
   ([remap open-line] . open-line-indent)
   ([remap set-mark-command] . set-mark-or-mark-line)
   ([remap kill-buffer] . kill-buffer-and-window)
   ([remap indent-region] . indent-region-dwim)
   ([remap kmacro-end-or-call-macro-repeat] . kmacro-end-or-call-macro-dwim)
   ([remap zap-to-char] . zap-up-to-char)

   ("C-u" . (lambda () (interactive) (set-mark-command 1)))
   ("C-z" . repeat)
   ("C-<tab>" . window-alternate-buffer)
   ("M-w" . kill-ring-save-region-or-next-kill)

   ("M-'" . jump-to-register)
   ("M-#" . point-to-register)

   :map ctl-x-map
   ("f" . find-file)
   ("j" . dired-jump)

   :map ctl-x-x-map
   ("f" . global-font-lock-mode)

   :map indent-rigidly-map
   ("C-i" . indent-rigidly-right-to-tab-stop)
   ("C-S-i" . indent-rigidly-left-to-tab-stop)
   ("SPC" . indent-rigidly-right)
   ("DEL" . indent-rigidly-left)))

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
	([remap dired-hide-details-mode] . (lambda ()
					     (interactive)
					     (mapc 'call-interactively
						   '(dired-omit-mode
						     dired-hide-details-mode))))))

(use-package org
  :init
  (setq org-tags-column 0))

(use-package package
  :init
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

(use-package puni
  :ensure t
  :init (puni-global-mode 1)

  :config
  (advice-add 'puni-mark-list-around-point :around
	      (lambda (fn &rest args)
		(unless (region-active-p)
		  (push-mark))
		(apply fn args)))

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

   :map lisp-mode-shared-map
   ("C-c ?" . puni-convolute)

   :map global-map
   ("C-<backspace>" . puni-backward-kill-to-indent)))

(use-package magit
  :ensure t
  :hook (magit-mode-hook . font-lock-mode)
  :bind (:map magit-mode-map ("C-<tab>" . window-alternate-buffer)))
