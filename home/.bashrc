#!/usr/bin/env bash

#  ---
#  title:  non-interactive bashrc
#  author: emil lenz
#  email:  emillenz@protonmail.com
#  date:   2024-11-30
#  info:

# since we use emacs (and it's tools like dired) instead of a terminal, we don't
# configure usability features (eg. autocomplete, syntax highlighting,
# vi-bindings), since these are all provided much more consistent and powerful
# by emacs.  this is the same reason we don't use zsh or fish.

# we NEVER write shell-script, instead we switch over to writing/prototyping in
# ruby (or python if it must be portable).  as soon as you need to use
# programming constructs or sed/awk..., it's time to switch.

#  we only configure environtment variables and aliases (more sane defaults)

#  ---

export PATH="$PATH:$HOME/.config/emacs/bin"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.local/share/gem/ruby/3.3.0/bin"

export EDITOR="emacsclient -nw --alternate-editor='emacs -nw'"
export VISUAL="emacsclient --reuse-frame --alternate-editor=emacs"
export BROWSER="firefox"

alias ls="ls -v --human-readable --group-directories-first --color=auto"
alias rm="rm --recursive --verbose"
alias du="du --human-readable"
alias mv="mv --verbose"
alias cp="cp --recursive --verbose"
alias dnf="sudo dnf --assumeyes"
alias curl="curl --silent"
alias echo="echo -e"

# using empty line to clearly separate commands in history.
# (if you customize this, ensure the prompt is still recognized by emacs comint-prompt-regexp.  (color & bold done diplayed using emacs))
PS1='\n $(pwd | sed "s,^$HOME,~,") > '
