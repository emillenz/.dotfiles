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

  (setopt history-length 1000
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
	  kill-whole-line t
	  shift-select-mode nil
	  set-mark-command-repeat-pop t

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
	  backup-directory-alist (list (cons "." (concat user-emacs-directory "backups")))

	  initial-scratch-message nil
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

	  tab-always-indent 'complete
	  indent-tabs-mode t
	  tab-width 8
	  standard-indent 8
	  c-default-style "linux"

	  compilation-scroll-output t
	  next-error-recenter '(4)

	  scroll-preserve-screen-position t
	  scroll-error-top-bottom t
	  auto-window-vscroll nil

	  comint-input-ignoredups t
	  comint-prompt-read-only t)

  (progn
    (global-hl-line-mode)
    (add-hook 'deactivate-mark-hook (lambda () (global-hl-line-mode 1)))
    (add-hook 'activate-mark-hook (lambda () (global-hl-line-mode -1))))

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

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

  (mapc (lambda (fn)
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
				(= (mark) (point)))
		      (push-mark)))))

    (mapc 'push-mark-once '(mark-paragraph backward-up-list)))

  (mapc (lambda (cmd)
	  (advice-add cmd :around
		      (lambda (fn &rest args)
			(with-undo-amalgamate
			  (apply fn args)))))
	'(kmacro-end-or-call-macro-repeat
	  query-replace-regexp))

  (progn
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

    (defun switch-to-other-buffer ()
      (interactive)
      (let ((buf (caar (window-prev-buffers))))
	(switch-to-buffer (unless (eq buf (current-buffer)) buf))))

    (defun buffer-to-register (reg)
      (interactive (list (register-read-with-preview "buffer to register:")))
      (set-register reg (cons 'buffer (current-buffer))))

    (progn
      (defmacro keymap-set! (keymap &rest pairs)
	(macroexp-progn
	 (cl-loop for (key cmd)
		  on pairs
		  by 'cddr
		  collect (list 'keymap-set keymap key cmd))))

      (keymap-set! global-map
		   "<remap> <downcase-word>" 'downcase-dwim
		   "<remap> <yank>" 'yank-indent
		   "<remap> <indent-for-tab-command>" 'indent-dwim
		   "<remap> <upcase-word>" 'upcase-dwim
		   "<remap> <capitalize-word>" 'capitalize-dwim
		   "<remap> <dabbrev-expand>" 'hippie-expand
		   "<remap> <eval-last-sexp>" 'eval-last-sexp-dwim
		   "<remap> <comment-dwim>" 'comment-sexp-dwim
		   "<remap> <open-line>" 'open-line-indent
		   "<remap> <zap-to-char>" 'zap-up-to-char
		   "<remap> <kill-buffer>" 'kill-buffer-and-window
		   "<remap> <list-buffers>" 'ibuffer
		   "<remap> <dired>" 'dired-jump
		   "<remap> <delete-horizontal-space>" 'cycle-spacing

		   "<remap> <shell-command>" 'async-shell-command
		   "<remap> <dired-do-shell-command>" 'dired-do-async-shell-command

		   "C-u" (lambda () (interactive) (set-mark-command 1))
		   "C-z" 'repeat
		   "M-w" 'kill-ring-save-region-or-next-kill
		   "M-j" 'jump-to-register
		   "C-<tab>" 'switch-to-other-buffer)

      (keymap-set! ctl-x-map
		   "t" 'recentf-open
		   "f" 'find-file)

      (keymap-set! ctl-x-x-map
		   "f" 'global-font-lock-mode)

      (keymap-set! ctl-x-r-map
		   "u" 'buffer-to-register)

      (keymap-set! indent-rigidly-map
		   "C-i" 'indent-rigidly-right-to-tab-stop
		   "C-M-i" 'indent-rigidly-left-to-tab-stop
		   "SPC" 'indent-rigidly-right
		   "DEL" 'indent-rigidly-left)

      (keymap-set! next-error-repeat-map
		   "<" 'first-error))))

(use-package isearch
  :config
  (add-hook 'isearch-update-post-hook
	    (lambda ()
	      (when (and isearch-other-end
			 isearch-forward
			 ;; neccessary, otherwise isearch won't exit on any non-isearch command
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
    (defvar ignore-self-insert-map
      (let ((map (make-keymap)))
	(set-char-table-range (nth 1 map) t 'ignore)
	map))
    (set-keymap-parent query-replace-map ignore-self-insert-map))

  (keymap-set! global-map
	       "<remap> <query-replace>" 'query-replace-regexp
	       "<remap> <isearch-query-replace>" 'isearch-query-replace-regexp)

  (keymap-set! query-replace-map
	       "p" 'backup))

(use-package icomplete
  :config
  (fido-vertical-mode)

  (setopt max-mini-window-height 12
	  completions-detailed t
	  completions-auto-help 'visible
	  completions-max-height 16
	  completions-format 'one-column
	  icomplete-compute-delay 0
	  read-buffer-completion-ignore-case t
	  read-file-name-completion-ignore-case t)

  (keymap-set! minibuffer-mode-map
	 "<remap> <minibuffer-complete>" 'icomplete-force-complete
	 "<remap> <minibuffer-choose-completion>" 'icomplete-fido-exit))

(use-package recentf
  :config
  (recentf-mode 1)
  (setopt recentf-max-saved-items 300))

(use-package savehist
  :config
  (savehist-mode 1)
  (setopt savehist-save-minibuffer-history t
	  savehist-additional-variables
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
	  dired-recursive-copies 'always
	  dired-recursive-deletes 'always
	  dired-no-confirm '(uncompress move copy)
	  dired-create-destination-dirs 'ask
	  dired-vc-rename-file t)

  (keymap-set! dired-mode-map
	"b" 'dired-up-directory
	"e" 'wdired-change-to-wdired-mode
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
  (mapc 'push-mark-once '(puni-mark-list-around-point
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
  (advice-remove 'delete-other-windows 'current-window-only--delete-other-windows)
  (add-to-list 'display-buffer-alist (list (rx (seq "*")
					       (or "Completions"
						   "Register Preview")
					       (seq "*"))
					   'display-buffer-at-bottom)))

(use-package pdf-tools
  :ensure t
  :init (pdf-tools-install)
  :config
  (setopt pdf-view-display-size 'fit-height))
