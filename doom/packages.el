;; -*- no-byte-compile: t; -*-

(disable-packages! evil-escape          ; faster to remap <esc> to caps
                   solaire-mode         ; distracting visual sugar
                   evil-snipe ; usless bloat :: just use incremental-search in combo with f/t motions
                   evil-easymotion
                   which-key
                   evil-exchange) ; do this using vanilla vim's visual pasting

(package! dired-open)
(package! tldr)
(package! rainbow-mode)
(package! doct)
(package! org-super-agenda)
(package! org-tidy)
(package! org-fragtog)
(package! laas)
(package! modus-themes)
(package! devdocs)
(package! harpoon)
(package! nov)
(package! whisper :recipe (:host github :repo "natrys/whisper.el"))
