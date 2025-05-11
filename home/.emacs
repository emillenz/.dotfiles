;; ---
;; title: minimalist, distractionless vanilla emacs config
;; date: [2025-05-09]
;; author: emil lenz
;; email: emillenz@protonmail.com
;; ---

(setq debug-on-error t)

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

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (scroll-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (tooltip-mode -1)
  (global-visual-line-mode -1)

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
	auto-save-list-file-prefix
	(expand-file-name "autosave/" user-emacs-directory)
	use-short-answers t
	save-interprogram-paste-before-kill t
	require-final-newline t
	load-prefer-newer t
	backup-by-copying t
	shell-command-prompt-show-cwd t
	backup-directory-alist `(("." .
				  ,(concat user-emacs-directory "backups")))
	custom-file (expand-file-name "custom.el" user-emacs-directory)
	display-line-numbers-type 'relative
	find-file-visit-truename t
	comment-empty-lines nil
	register-preview-delay nil
	icomplete-compute-delay 0.01
	auto-window-vscroll nil
	kill-do-not-save-duplicates t
	show-paren-when-point-inside-paren t

	compilation-scroll-output t
	next-error-recenter '(4)

	create-lockfiles nil
	make-backup-files nil

	vc-follow-symlinks t
	vc-make-backup-files nil

	scroll-preserve-screen-position t
	scroll-error-top-bottom t

	isearch-allow-motion t
	isearch-motion-changes-direction t

	frame-title-format "%b ― Emacs"
	icon-title-format "%b ― Emacs"
	frame-inhibit-implied-resize t

	comint-input-ignoredups t
	comint-prompt-read-only t)

  (dolist (cmd '(list-timers narrow-to-region upcase-region downcase-region
                             erase-buffer scroll-left dired-find-alternate-file))
    (put cmd 'disabled nil))

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (progn
    (setq minibuffer-prompt-properties
	  '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode))

  (progn
    (set-language-environment "UTF-8")
    (setq default-input-method nil))

  (setq-default comment-column 0)

  (global-font-lock-mode -1)

  (add-to-list 'default-frame-alist '(font . "Iosevka Comfy-10"))

  (progn
    (require-theme 'modus-themes)
    (setq modus-themes-italic-constructs t
	  modus-themes-bold-constructs t
	  modus-themes-common-palette-overrides '((fg-heading-1 fg-heading-0)
						  (bg-prose-block-contents bg-dim)))
    (load-theme 'modus-operandi))

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
			   (seq "Org " (or "Select" "todo"))
			   "Agenda Commands"
			   "Completions")))
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
      (setq Man-notify-method 'pushy))
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

  (advice-add 'backward-up-list :around
	      (lambda (fn &rest args)
		(unless (eq last-command 'backward-up-list) (push-mark))
		(apply fn args)))

  (defun set-mark-or-mark-line (arg)
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

  (defun kill-ring-save-sexp (arg)
    (interactive "p")
    (if (region-active-p)
	(kill-ring-save (region-beginning) (region-end))
      (save-mark-and-excursion
	(mark-sexp arg)
	(kill-ring-save (region-beginning) (region-end)))))

  (defun open-line-indented (arg)
    (interactive "p")
    (save-mark-and-excursion
      (open-line arg)
      (forward-line arg)
      (indent-according-to-mode)))

  (defun indent-region-dwim (arg)
    (interactive "p")
    (if (region-active-p)
	(indent-region (region-beginning) (region-end))
      (save-mark-and-excursion
	(mark-paragraph arg)
	(indent-region (region-beginning) (region-end)))))

  (defun eval-last-sexp-dwim (arg)
    (interactive "p")
    (if (region-active-p)
	(eval-region (region-beginning) (region-end) t)
      (call-interactively 'eval-last-sexp)))

  (defun kmacro-end-and-call-macro-dwim (arg)
    (interactive "p")
    (if (region-active-p)
	(apply-macro-to-region-lines (region-beginning) (region-end))
      (kmacro-end-and-call-macro arg)))

  :bind
  (([remap downcase-word] . downcase-dwim)
   ([remap upcase-word] . upcase-dwim)
   ([remap capitalize-word] . capitalize-dwim)
   ([remap dabbrev-expand] . hippie-expand)
   ([remap list-buffers] . ibuffer)
   ([remap eval-last-sexp] . eval-last-sexp-dwim)
   ([remap open-line] . open-line-indented)
   ([remap set-mark-command] . set-mark-or-mark-line)
   ([remap kill-buffer] . kill-buffer-and-window)
   ([remap indent-region] . indent-region-dwim)
   ([remap kmacro-end-and-call-macro] . kmacro-end-and-call-macro-dwim)

   ("C-u" . (lambda () (interactive) (set-mark-command 1)))
   ("C-z" . repeat)

   ("M-'" . jump-to-register)
   ("M-#" . point-to-register)

   ("C-x f" . find-file)
   ("C-x j" . dired-jump)
   ("C-x O" . delete-other-windows)

   ("C-<tab>" . (lambda ()
		  (interactive)
		  (switch-to-buffer (other-buffer))))))

(use-package package
  :init
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

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
  :init
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'dired-omit-mode)

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
	([remap dired-hide-details-mode] .
	 (lambda () (interactive)
	   (mapc 'call-interactively '(dired-omit-mode dired-hide-details-mode))))))

(use-package puni
  :ensure t
  :init (puni-global-mode 1)

  :bind (("C-M-r" . puni-raise)
	 ("C-M-s" . puni-splice)
	 ("C-(" . puni-slurp-backward)
	 ("C-)" . puni-slurp-forward)
	 ("C-{" . puni-barf-backward)
	 ("C-}" . puni-barf-forward)

	 :map lisp-mode-shared-map
	 ("C-c v" . puni-convolute)))

(use-package whole-line-or-region
  :ensure t
  :after puni
  :init (whole-line-or-region-global-mode 1)
  :bind
  (([remap puni-kill-region] . (lambda (arg)
				 (interactive "p")
				 (if (region-active-p)
				     (puni-kill-region)
				   (whole-line-or-region-kill-region arg))))))

(use-package magit
  :ensure t
  :bind
  (:map magit-mode-map
	("C-<tab>" . (lambda ()
		       (interactive)
		       (switch-to-buffer (other-buffer nil))))))
