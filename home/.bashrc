#!/usr/bin/env bash

#  ---
#  title:  minimal bashrc
#  author: emil lenz
#  email:  emillenz@protonmail.com
#  date:   2024-11-30
#  info:

# since we use emacs's shell with comint-mode (and tools like magit, dired over git, cd) and thus never work with a shell inside an actual terminal.  we don't need bloated frameworks, plugins, syntax highlighting, prompt-themes etc...  we don't use fancy "CLI" enhancements like eza, z-jump...

# we NEVER write POSIX shell scripts, instead we switch over to writing/prototyping in clojure/babashka (or python if it must be portable).  if you want to do more than oneliner, it's time to switch.

#  ---

export GOPATH="$HOME/.local/share/go"

export PATH="$PATH:$GOPATH"
export PATH="$PATH:$HOME/.config/emacs/bin"
export PATH="$PATH:$HOME/.cargo/bin"

export EDITOR="emacsclient -nw --alternate-editor='emacs -nw'"
export VISUAL="emacsclient --reuse-frame --alternate-editor=emacs"
export BROWSER="firefox"

alias ls="ls --human-readable --group-directories-first --color=auto"
alias rm="rm --recursive --verbose"
alias du="du --human-readable"
alias mv="mv --verbose"
alias cp="cp --recursive --verbose"
alias echo="echo -e"
alias dnf="sudo dnf --assumeyes"

shopt -s nocaseglob
shopt -s histappend
export HISTCONTROL=ignoredups

# minimal, functional prompt, (no visual sugar.  same as emacs).  using empty line to clearly separate commands in history.
PS1='\n \W > '

# for when you are forced outside of emacs ~shell~.
set -o vi
