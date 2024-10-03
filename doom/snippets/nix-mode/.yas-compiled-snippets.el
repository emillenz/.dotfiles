;;; Compiled snippets and support files for `nix-mode'
;;; contents of the .yas-setup.el support file:
;;;
;;; ~/work/conf/doom-emacs-private/snippets/nix-mode/.yas-setup.el -*- lexical-binding: t; -*-

(defun %nix-mode-if-parent (name &optional else)
  (if (%nix-mode-in-block-p name)
      (or else "")
    (concat name ".")))

(defun %nix-mode-in-block-p (name)
  (if (not (fboundp 'evil-previous-open-brace))
      nil ; TODO Add non-evil support
    (save-excursion
      (evil-previous-open-brace)
      (re-search-backward (format "^\\s-*%s\\s-*=" name) (line-beginning-position) t))))
;;; Snippet definitions:
;;;
(yas-define-snippets 'nix-mode
                     '(("wsb" "pkgs.writeScriptBin \"${1:name}\" ''\n  #!${pkgs.stdenv.shell}\n  ${0:`%`}\n''" "pkgs.writeScriptBin ..." nil nil nil "/home/lenz/.config/doom/snippets/nix-mode/writeScriptBin" nil "wsb")
                       ("with" "with ${1:pkgs};" "with ...;" nil nil nil "/home/lenz/.config/doom/snippets/nix-mode/with" nil nil)
                       ("pkgu" "{ stdenv, lib, fetchurl$1 }:\n\nstdenv.mkDerivation rec {\n  version = \"$2\";\n  name = \"$3-\\$\\{version\\}\";\n\n  src = fetchurl {\n    url = \"$4\";\n    sha256 = \"$5\";\n  };\n\n  buildInputs = [ $1 ];\n\n  meta = {\n    description = \"$6\";\n    homepage = \"https://$7\";\n    license = lib.licenses.${8:$$\n    (yas-choose-value '(\n      \"agpl3\"\n      \"asl20\"\n      \"bsd2\"\n      \"bsd3\"\n      \"gpl2\"\n      \"gpl3\"\n      \"lgpl3\"\n      \"mit\"\n    ))};\n    maintainers = [ lib.maintainers.$9 ];\n    platforms = platforms.${10:$$\n    (yas-choose-value '(\n      \"gnu\"\n      \"linux\"\n      \"darwin\"\n      \"freebsd\"\n      \"openbsd\"\n      \"netbsd\"\n      \"cygwin\"\n      \"illumos\"\n      \"unix\"\n      \"all\"\n      \"none\"\n      \"allBut\"\n      \"mesaPlatforms\"\n      \"x86\"\n      \"i686\"\n      \"arm\"\n      \"mips\"\n    ))};\n  };\n}" "package url" nil nil nil "/home/lenz/.config/doom/snippets/nix-mode/package.url" nil "pkgu")
                       ("pkgg" "{ stdenv, lib, fetchFromGitHub$1 }:\n\nstdenv.mkDerivation rec {\n  name = \"$2-\\$\\{version\\}\";\n  version = \"$3\";\n\n  src = fetchFromGitHub {\n    owner = \"$4\";\n    repo = \"$2\";\n    rev = \"${5:v\\$\\{version\\}}\";\n    sha256 = \"$6\";\n  };\n\n  buildInputs = [ $1 ];\n\n  meta = {\n    description = \"$7\";\n    homepage = \"https://${8:github.com/$4/$2}\";\n\n    license = lib.licenses.${9:$$\n    (yas-choose-value '(\n      \"agpl3\"\n      \"asl20\"\n      \"bsd2\"\n      \"bsd3\"\n      \"gpl2\"\n      \"gpl3\"\n      \"lgpl3\"\n      \"mit\"\n    ))};\n    maintainers = [ lib.maintainers.$10 ];\n    platforms = lib.platforms.${11:$$\n    (yas-choose-value '(\n      \"gnu\"\n      \"linux\"\n      \"darwin\"\n      \"freebsd\"\n      \"openbsd\"\n      \"netbsd\"\n      \"cygwin\"\n      \"illumos\"\n      \"unix\"\n      \"all\"\n      \"none\"\n      \"allBut\"\n      \"mesaPlatforms\"\n      \"x86\"\n      \"i686\"\n      \"arm\"\n      \"mips\"\n    ))};\n  };\n}" "package github" nil nil nil "/home/lenz/.config/doom/snippets/nix-mode/package.github" nil "pkgg")
                       ("imp" "imports = [\n  ${0:`%`}\n];" "imports = [ ... ];"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/nix-mode/imports" nil "imp")
                       ("hmu" "home-manager.users.`user-login-name`" "home-manager.users.$USER" nil nil nil "/home/lenz/.config/doom/snippets/nix-mode/home-manager.users.USER" nil "hmu")
                       ("hm" "home-manager" "home-manager" nil nil nil "/home/lenz/.config/doom/snippets/nix-mode/home-manager" nil "hm")
                       ("ftb" "builtins.fetchTarball ${1:url}" "builtins.fetchTarball ..." nil nil nil "/home/lenz/.config/doom/snippets/nix-mode/fetchTarball" nil "ftb")
                       ("eva" "`(%nix-mode-if-parent \"environment\")`variables = {\n  ${0:`%`}\n};" "environment.variables" nil nil nil "/home/lenz/.config/doom/snippets/nix-mode/environment.variables" nil "eva")
                       ("esp" "`(%nix-mode-if-parent \"environment\")`systemPackages = with pkgs; [\n  ${0:`%`}\n];" "environment.systemPackages" nil nil nil "/home/lenz/.config/doom/snippets/nix-mode/environment.systemPackages" nil "esp")
                       ("esv" "`(%nix-mode-if-parent \"environment\")`sessionVariables = {\n  ${0:`%`}\n};" "environment.sessionVariables" nil nil nil "/home/lenz/.config/doom/snippets/nix-mode/environment.sessionVariables" nil "esv")
                       ("env" "environment = {\n  ${0:`%`}\n};" "environment" nil nil nil "/home/lenz/.config/doom/snippets/nix-mode/environment" nil "env")))


;;; Do not edit! File generated at Thu Apr 25 09:20:48 2024
