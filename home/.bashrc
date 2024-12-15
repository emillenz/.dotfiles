#!/usr/bin/env bash

# ---
# title:  minimalist bashrc
# author: emil lenz
# email:  emillenz@protonmail.com
# date:   2024-11-30
# ---

export GOPATH="$HOME/.local/share/go"

export PATH="$PATH:$GOPATH"
export PATH="$PATH:$HOME/.config/emacs/bin"
export PATH="$PATH:$HOME/.cargo/bin"

export BROWSER="firefox"
export EDITOR="vi"
export VISUAL="vi"
alias vi="vi -u ~/.vimrc" # HACK :: using vim-minimal => must explicitly source vimrc file

alias ls="ls --human-readable --group-directories-first --color=always"
alias rm="rm --recursive --verbose"
alias du="du --human-readable"
alias mv="mv --verbose"
alias cp="cp --recursive --verbose"
alias echo="echo -e"
alias dnf="sudo dnf --assumeyes"

shopt -s globstar
shopt -s histappend
shopt -s checkjobs
export HISTCONTROL=ignoredups

# case insensitive matching by default
shopt -s nocaseglob
shopt -s nocasematch
alias grep="grep --ignore-case"

# minimal, functional prompt :: 
# - newline to separate command outputs in history
# - bold to make prompts more visually distinct
PS1=$'\n\[\033[1m\][\W] > \[\033[0m\]'
