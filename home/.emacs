;; ---
;; title: minimalist, focused emacs config.
;; date: [2025-04-08]
;; author: emil lenz
;; email: emillenz@protonmail.com
;; ---

(use-package emacs
  :init
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (fringe-mode 1)
  (blink-cursor-mode 0)
  (scroll-bar-mode 0)
  (horizontal-scroll-bar-mode 0)
  (global-hl-line-mode 0)
  (global-display-line-numbers-mode 0)
  (global-auto-revert-mode 1)
  (column-number-mode 1)
  (savehist-mode 1)
  (global-word-wrap-whitespace-mode 1)
  (global-visual-line-mode 1)
  (save-place-mode 1)
  (global-subword-mode 1)
  (delete-selection-mode 1)
  (electric-pair-mode 1)
  (repeat-mode 1)
  (fido-vertical-mode 1)
  (desktop-save-mode 1)

  (setq use-file-dialog nil
	use-dialog-box nil
	set-mark-command-repeat-pop t
	history-length 1000
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
	display-bine-numbers-type 'relative
	vc-follow-symlinks t
	create-lockfiles nil
	make-backup-files nil
	comint-input-ignoredups t
	comint-prompt-read-only t
	use-package-hook-name-suffix nil
	comment-empty-lines nil
	register-preview-delay nil)

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
    (electric-indent-mode 1)
    (setq-default tab-always-indent 'complete
		  indent-tabs-mode t
		  tab-width 8
		  standard-indent 8)
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

  (thread-last '(apply-macro-to-region-lines
		 eval-region
		 align
		 align-entire)
	       (mapc (lambda (fn)
		       (advice-add fn :after
				   (lambda (&rest r)
				     (deactivate-mark))))))

  (defun eval-last-sexp-dwim ()
    (interactive)
    (call-interactively (if (region-active-p)
			    'eval-region
			  'eval-last-sexp)))

  (defun indent-region-dwim (arg)
    (interactive "p")
    (if (region-active-p)
	(indent-region (point) (mark))
      (save-mark-and-excursion
	(mark-paragraph arg)
	(indent-region (point) (mark)))))

  (defun kill-ring-save-sexp (arg)
    (interactive "p")
    (save-mark-and-excursion
      (mark-sexp arg)
      (kill-ring-save (point)
		      (mark))))

  (defun open-line-indented (arg)
    (interactive "p")
    (save-mark-and-excursion
      (open-line arg)
      (forward-line arg)
      (indent-according-to-mode)))

  (defun kmacro-call-macro-dwim (arg)
    (interactive "p")
    (call-interactively
     (if (region-active-p)
	 'apply-macro-to-region-lines
       'kmacro-end-and-call-macro)))

  :bind
  (([remap downcase] . downcase-dwim)
   ([remap upcase] . upcase-dwim)
   ([remap list-buffers] . ibuffer)
   ([remap dabbrev-expand] . hippie-expand)
   ([remap eval-last-sexp] . eval-last-sexp-dwim)
   ([remap capitalize-dwim] . capitalize-dwim)
   ([remap indent-region] . indent-region-dwim)
   ([remap open-line] . open-line-indented)
   ([remap kmacro-end-and-call-macro] . kmacro-call-macro-dwim)
   ([remap kill-buffer] . (lambda ()
			    (interactive)
			    (kill-buffer nil)))

   ("C-u" . (lambda () (interactive) (set-mark-command 4)))
   ("C-<tab>" . (lambda () (interactive) (switch-to-buffer nil)))

   ("M-'" . jump-to-register)
   ("M-#" . point-to-register)
   ;; ("M-j" . jump-to-register-buffer) ;; TODO

   ("C-M-y" . append-next-kill)
   ("C-M-w" . kill-ring-save-sexp)

   ("C-x o" . delete-window)
   ("C-x f" . find-file)
   ("C-x j" . dired-jump))

  :hook
  ((before-save-hook . delete-trailing-whitespace)))

(use-package dired
  :init
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (setq dired-omit-files "^\\..*$"
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
					     (thread-last '(dired-omit-mode 
							    dired-hide-details-mode)
							  (mapc 'call-interactively))))))

(use-package puni
    :ensure t
    :init (puni-global-mode 1)
    :bind (("C-M-r" .	puni-raise)
	   ("C-M-s" . puni-splice)
	   ("C-{" . puni-slurp-backward)
	   ("C-}" . puni-barf-backward)
	   ("C-)" . puni-slurp-forward)
	   ("C-(" . puni-barf-forward)
	   ("C-c ?" . puni-convolute)))

(use-package whole-line-or-region
  :ensure t
  :after puni
  :init (whole-line-or-region-global-mode 1)

  :config
  (defun whole-line-or-puni-kill-region (arg)
    (interactive "p")
    (call-interactively
     (if (region-active-p)
	 'puni-kill-region
       'whole-line-or-region-kill-region)))
  :bind
  (([remap puni-kill-region] . whole-line-or-puni-kill-region)))

(use-package magit
  :ensure t)
