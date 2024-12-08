;; -*- no-byte-compile: t; -*-

(disable-packages! evil-escape           ;; remap caps->esc
		   ;; distracting visual sugar
		   solaire-mode
		   ;; usless bloat :: just use incremental-search in combo with f/t motions
		   evil-snipe
		   evil-easymotion

		   ;; thats what tabs are for
		   evil-lion

		   ;; we use lispyville which is made for evil (don't know why doom includes lispy)
		   lispy
		   ;; this is the stupid bloat we get when people don't understand how to use evil's registers (hint: use evil's yank register ~"0~)
		   evil-exchange)

(package! dired-open)
(package! tldr)
(package! rainbow-mode)
(package! doct)
(package! org-super-agenda)
(package! org-tidy)
(package! org-fragtog)
(package! laas)
;; NOTE :: is already part of emacs, but we want the upstream version
(package! modus-themes)
(package! devdocs)
(package! nov)
(package! harpoon)
(package! whisper :recipe (:host github :repo "natrys/whisper.el"))
