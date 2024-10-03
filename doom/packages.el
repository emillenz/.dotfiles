;; -*- no-byte-compile: t; -*-

(disable-packages! evil-escape  ;; faster to remap <esc> to caps
                   ;; which-key    ;; bad for muscle memory & slow to lookup
                   solaire-mode ;; distracting visual sugar
                   evil-snipe    ;; faster to just use incsearch immeadiately (or f/t is enough)
                   evil-exchange) ;; do this using vanilla vim's visual pasting

(package! dired-open)
(package! rainbow-mode)
(package! doct)
(package! org-super-agenda)
(package! org-tidy)
(package! org-fragtog)
(package! laas)
(package! modus-themes)
(package! verilog-mode)
(package! devdocs)
(package! harpoon)
