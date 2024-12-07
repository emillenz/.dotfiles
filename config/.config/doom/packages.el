;; -*- no-byte-compile: t; -*-

(disable-packages! evil-escape          ; faster to remap <esc> to caps
                   solaire-mode         ; distracting visual sugar
                   evil-snipe           ; usless bloat :: just use incremental-search in combo with f/t motions
                   evil-easymotion
                   lispy                ;; we use lispyville which is made for evil (don't know why doom includes lispy)
                   evil-exchange)       ; this is the stupid bloat we get when people don't understand how to use evil's registers (hint: use evil's yank register ~"0~)

(package! dired-open)
(package! tldr)
(package! rainbow-mode)
(package! doct)
(package! org-super-agenda)
(package! org-tidy)
(package! org-fragtog)
(package! laas)
(package! modus-themes)                 ;; NOTE :: is already part of emacs, but we want the upstream version
(package! devdocs)
(package! nov)
(package! whisper :recipe (:host github :repo "natrys/whisper.el"))
(package! harpoon)
