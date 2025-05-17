;; -*- lexical-binding: t -*-
;; ---
;; title: minimalist, distractionless vanilla emacs config
;; date: [2025-05-09]
;; author: emil lenz
;; email: emillenz@protonmail.com
;; ---

(setopt use-package-hook-name-suffix nil
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
  (save-place-mode)
  (auto-revert-mode)
  (global-visual-line-mode)
  (auto-save-visited-mode)
  (fringe-mode 1)

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
	  mark-even-if-inactive nil
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
	  backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))

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
    (global-font-lock-mode -1))

  (progn
    (modify-syntax-entry ?. "_" (standard-syntax-table))
    (modify-syntax-entry ?: "_" (standard-syntax-table)))

  (put 'narrow-to-region 'disabled nil)

  (mapc (lambda (fn)
	  (advice-add fn
		      :around
		      (lambda (fn &rest args)
			(apply fn args)
			(deactivate-mark))))
	'(apply-macro-to-region-lines
	  eval-region
	  align
	  align-entire))

  (progn
    (mapc (lambda (command)
	    (advice-add command
			:around
			(lambda (fn &rest args)
			  (unless (eq last-command this-command)
			    (push-mark))
			  (apply fn args))))
	  '(mark-paragraph
	    backward-up-list
	    puni-end-of-sexp
	    puni-beginning-of-sexp))

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

  (defun switch-to-other-buffer ()
    (interactive)
    (let ((buf (caar (window-prev-buffers))))
      (switch-to-buffer (unless (eq buf (current-buffer)) buf))))

  (defun buffer-to-register (reg)
    (interactive (list (register-read-with-preview "buffer to register:")))
    (set-register reg `(buffer . ,(current-buffer))))

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
  ([remap zap-to-char] . zap-up-to-char)
  ([remap kill-buffer] . kill-buffer-and-window)
  ([remap list-buffers] . ibuffer)
  ([remap dired] . dired-jump)

  ([remap dired-shell-command] . dired-async-shell-command)
  ([remap shell-command] . async-shell-command)

  ("C-u" . (lambda () (interactive) (set-mark-command 1)))
  ("C-z" . repeat)
  ("M-w" . kill-ring-save-region-or-next-kill)
  ("M-o" . switch-to-other-buffer)

  ("M-SPC" . mark-word)
  ([remap delete-horizontal-space] . cycle-spacing)

  ("M-j" . jump-to-register)

  (:map ctl-x-map
	("t" . recentf-open)
	("f" . find-file))

  (:map ctl-x-x-map
	("f" . global-font-lock-mode))

  (:map ctl-x-r-map
	("u" . buffer-to-register))

  (:map indent-rigidly-map
	("C-i" . indent-rigidly-right-to-tab-stop)
	("C-S-i" . indent-rigidly-left-to-tab-stop)
	("SPC" . indent-rigidly-right)
	("DEL" . indent-rigidly-left))

  (:repeat-map comint-repeat-map
	       ("M-s" . comint-next-matching-input-from-input)
	       ("M-r" . comint-previous-matching-input-from-input))

  (:repeat-map next-error-repeat-map
	       ("M-<" . first-error)))

(use-package isearch
  :hook
  ((isearch-update-post-hook
    . (lambda ()
	(when (and isearch-other-end
		   isearch-forward
		   ;; neccessary, otherwise isearch won't exit on any non-isearch command
		   (string-prefix-p "isearch" (symbol-name last-command)))
	  (goto-char isearch-other-end)))))

  :init
  (setopt isearch-lax-whitespace t
	  search-whitespace-regexp ".*?"

	  lazy-highlight-initial-delay 0
	  isearch-lazy-count t
	  isearch-allow-motion t
	  isearch-motion-changes-direction t)

  (keymap-unset isearch-mode-map "C-w" t)
  (keymap-unset isearch-mode-map "C-M-d" t)

  :bind
  (([remap isearch-delete-char] . isearch-del-char)))

(use-package replace
  :init
  (progn
    (defvar ignore-self-insert-map
      (let ((map (make-keymap)))
	(set-char-table-range (nth 1 map) t 'ignore)
	map))
    (set-keymap-parent query-replace-map ignore-self-insert-map))

  :bind
  (([remap query-replace] . query-replace-regexp)
   ([remap isearch-query-replace] . isearch-query-replace-regexp)

   (:map query-replace-map
	 ("p" . backup))))

(use-package icomplete
  :init
  (fido-vertical-mode)

  (setopt max-mini-window-height 12
	  completions-detailed t
	  completions-auto-help 'visible
	  completions-max-height 16
	  completions-format 'one-column
	  icomplete-compute-delay 0
	  read-buffer-completion-ignore-case t
	  read-file-name-completion-ignore-case t)

  :bind
  ((:map minibuffer-mode-map
	 ([remap minibuffer-complete] . icomplete-force-complete)
	 ([remap minibuffer-choose-completion] . icomplete-fido-exit))))

(use-package recentf
  :init
  (recentf-mode 1)
  (setopt recentf-max-saved-items 300))

(use-package savehist
  :init
  (savehist-mode 1)
  (setopt savehist-save-minibuffer-history t
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
  (setopt dired-free-space nil
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

(use-package org
  :init
  (setopt org-tags-column 0))

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
	      (lambda (fn &rest args)
		(unless (region-active-p)
		  (push-mark))
		(apply fn args)))

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
  :hook ((magit-mode-hook . font-lock-mode)))

(use-package current-window-only
  :ensure t
  :init (current-window-only-mode 1)
  :config
  (advice-remove 'delete-other-windows 'current-window-only--delete-other-windows)
  (add-to-list 'display-buffer-alist '("*Completions*" display-buffer-at-bottom)))

(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install)
  (setopt pdf-view-display-size 'fit-height))
