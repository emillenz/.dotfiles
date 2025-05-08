;; -*- lexical-binding: t; -*-

;; ---
;; title: minimalist, focused emacs config.
;; date: [2025-04-08]
;; author: emil lenz
;; email: emillenz@protonmail.com
;; ---

(progn
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (fringe-mode 1)
  (blink-cursor-mode 0)
  (scroll-bar-mode 0)
  (horizontal-scroll-bar-mode 0)
  (global-hl-line-mode 0)
  (global-display-line-numbers-mode 1)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (global-auto-revert-mode 1)
  (column-number-mode 1)
  (savehist-mode 1)
  (global-word-wrap-whitespace-mode 1)
  (save-place-mode 1)
  (global-subword-mode 1)
  (repeat-mode 1)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (setq set-mark-command-repeat-pop t
	history-length 1000
	comment-empty-lines t
	uniquify-buffer-name-style 'forward
	delete-by-moving-to-trash t
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
	inhibit-startup-screen t
	initial-scratch-message nil
	display-line-numbers-type 'relative
	vc-follow-symlinks t
	create-lockfiles nil
	make-backup-files nil))

(progn (add-to-list 'default-frame-alist '(font . "Iosevka Comfy-10")))

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
  (with-eval-after-load 'evil
    (setq-default evil-shift-width 8))
  (setq c-default-style "linux"))

(progn
  (setq display-buffer-base-action '(display-buffer-reuse-window display-buffer-same-window)
	display-buffer-alist
	`((,(rx (seq "*"
		     (or "transient"
			 (seq "Org " (or "Select" "todo"))
			 "Agenda Commands"
			 "Completions"
			 (seq (opt "Async ") "Shell Command"))))
	   display-buffer-at-bottom
	   (window-height . fit-window-to-buffer))
	  ("." (display-buffer-reuse-window display-buffer-same-window)
	   (reuseable-frames . t))))

  (with-eval-after-load 'ediff (setq ediff-window-setup-function 'ediff-setup-windows-plain))
  (with-eval-after-load 'org (setq org-src-window-setup 'current-window
				   org-agenda-window-setup 'current-window))
  (with-eval-after-load 'man (setq Man-notify-method 'pushy))
  (advice-add 'switch-to-buffer-other-window :override 'switch-to-buffer)
  (advice-add 'pop-to-buffer :override 'switch-to-buffer))

(progn
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			   ("melpa" . "https://melpa.org/packages/")))
  (package-initialize))

(use-package evil
  :ensure t
  :after general
  :init
  (setq evil-search-module 'evil-search
	evil-want-keybinding nil
	evil-want-C-u-delete t
	evil-want-C-u-scroll t
	evil-want-C-h-delete t
	evil-want-Y-yank-to-eol t
	evil-repeat-move-cursor nil
	evil-undo-system 'undo-redo)
  (evil-mode 1)

  :config
  (setq evil-mode-line-format nil
	evil-insert-state-cursor '(box))

  (thread-last '(evil-yank
		 evil-indent
		 evil-shift-right
		 evil-shift-left
		 evil-downcase
		 evil-upcase
		 evil-invert-case)
	       (mapc (lambda (fn)
		       (advice-add fn :around
				   (lambda (og-fn &rest args)
				     (save-excursion (apply og-fn args))))
		       (evil-set-command-property fn :move-point nil))))

  (thread-last '(evil-ex-search-next
		 evil-ex-search-previous
		 evil-forward-paragraph
		 evil-backward-paragraph
		 evil-forward-sentence-begin
		 evil-backward-sentence-begin)
	       (mapc (lambda (fn)
		       (evil-set-command-property fn :jump nil))))

  (dolist (fn '(evil-next-line evil-previous-line))
    (advice-add fn :around
		(lambda (fn count)
		  (when count (evil-set-marker ?`))
		  (call-interactively fn))))

  (advice-add 'evil-force-normal-state :after 'evil-ex-nohighlight)

  (defun evil-normal-insert-newline-above (count)
    (interactive "p")
    (save-excursion (dotimes (_ count) (evil-insert-newline-above)))
    (when (bolp) (forward-char count)))

  (defun evil-normal-insert-newline-below (count)
    (interactive "p")
    (save-excursion (dotimes (_ count) (evil-insert-newline-below))))

  (evil-define-operator evil-query-replace (beg end type)
    :repeat t
    :move-point nil
    (interactive "<R>")
    (save-excursion
      (goto-char beg)
      (set-mark (point))
      (goto-char end)
      (condition-case nil
	  (call-interactively 'query-replace-regexp)
	(t (deactivate-mark)))))
  (general-define-key
   :states 'normal
   "&" 'evil-query-replace)

  (evil-define-operator evil-shell-cmd (beg end type)
    :repeat t
    :move-point nil
    (goto-char beg)
    (set-mark (point))
    (goto-char end)
    (condition-case nil
	(call-interactively 'shell-command-on-region)
      (t (deactivate-mark))))
  (general-define-key
   :states 'normal "!" 'evil-shell-cmd)

  (evil-define-operator evil-eval (beg end type)
    :repeat nil
    :move-point nil
    (interactive "<R>")
    (eval-region beg end t))

  (defun evil-global-mark-goto (char)
    (interactive (list (read-char)))
    (let ((char (upcase char))
	  (marker (evil-get-marker char)))
      (cond ((markerp marker) (switch-to-buffer (marker-buffer marker)))
	    ((consp marker) (find-file (car marker))))))
  (general-define-key [remap evil-goto-mark-line] 'evil-global-mark-goto))

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (setq evil-collection-setup-minibuffer t
	evil-collection-want-unimpaired-p nil)
  :config
  (evil-collection-init))

(use-package dired
  :ensure nil
  :after general
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'dired-omit-mode)

  (setq dired-omit-files "^\\..*$"
	dired-recursive-copies 'always
	dired-recursive-deletes 'always
	dired-no-confirm '(uncompress move copy)
	dired-create-destination-dirs 'ask
	dired-vc-rename-file t)

  (general-define-key
   [remap dired-hide-details-mode] (lambda ()
				     (interactive)
				     (call-interactively 'dired-hide-details-mode)
				     (call-interactively 'dired-omit-mode)))
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "h" 'dired-up-directory
   "l" 'dired-find-file))

(use-package icomplete
  :after general
  :init
  (fido-vertical-mode 1)

  (general-define-key
   :states 'insert
   :keymaps 'icomplete-fido-mode-map
   "C-i" 'icomplete-fido-ret
   "C-n" 'icomplete-forward-completions
   "C-p" 'icomplete-backward-completions))

(use-package general
  :ensure t
  :init
  (setq general-override-states '(insert
				  emacs
				  hybrid
				  normal
				  visual
				  motion
				  operator
				  replace))

  (general-define-key
    :states 'normal
    :keymaps 'override
    :prefix "SPC"
    "g" 'magit-status
    "p" project-prefix-map
    "v" vc-prefix-map
    "b" 'switch-to-buffer
    "k" 'kill-current-buffer
    "e" 'find-file
    "E" 'dired-jump
    "s" 'save-buffer
    "!" 'shell-command)

  (general-define-key
    :states 'normal
    :keymaps 'override
    :prefix ","
    "e" 'evil-eval)

  (general-define-key
   :states 'normal
   :keymaps '(minibuffer-mode-map evil-ex-search-keymap)
   "j" 'next-history-element
   "k" 'previous-history-element
   "/" 'previous-matching-history-element
   "RET" 'exit-minibuffer)

  (general-define-key
   :states 'normal
   :keymaps 'global
   "go" 'evil-normal-insert-newline-below
   "gO" 'evil-normal-insert-newline-above

   "[q" 'previous-error
   "]q" 'next-error
   "[Q" 'first-error

   "C-<tab>" 'evil-switch-to-windows-last-buffer
   ":" 'execute-extended-command
   "g/" 'occur
   "_" (lambda ()
	 (interactive)
	 (evil-use-register ?_)
	 (call-interactively 'evil-delete))
   "L" (lambda ()
	 (interactive)
	 (save-excursion (call-interactively 'newline-and-indent)))))

(use-package comint
  :ensure nil
  :after general
  :config
  (setq comint-input-ignoredups t
	comint-prompt-read-only t)

  (general-define-key
   :states 'insert
   :keymaps 'comint-mode-map
   "C-r" 'comint-history-isearch-backward-regexp)

  (general-define-key
   :states 'normal
   :keymaps 'comint-mode-map
   "RET" 'comint-send-input))

(use-package magit
  :ensure t
  :init
  (setq evil-collection-magit-use-z-for-folds t))
