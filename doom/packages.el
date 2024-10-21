;; -*- no-byte-compile: t; -*-

(disable-packages! evil-escape    ; faster to remap <esc> to caps
                   solaire-mode   ; distracting visual sugar
                   evil-snipe     ; usless bloat :: just use incsearch if f/t -motions are not enough.
                   evil-easymotion
                   evil-exchange) ; do this using vanilla vim's visual pasting

;; ESSENTIAL PACKAGES
(package! vertico :pin "4498a2589c60cc788d6cb3909964c12c8fbad79a") ;; HACK :: vertico upstream gives errors with doom, since it uses `static-if' which is only available in emacs 30.X which is currently unsupported by doom.  TODO check if this can be removed without vertico erroring
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

;; TEMPORARY PACKAGES
(package! verilog-mode)
