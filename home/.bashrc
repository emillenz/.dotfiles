#!/usr/bin/env bash

# ---
# title:  minimalist bashrc
# author: emil lenz
# email:  emillenz@protonmail.com
# date:   2024-11-30
# info:
#   - case insesitive :: configured if available (grep, find...)
#   - theme :: minimalist
#     - reduce colors
#     - no angry fruit-salad command output
#     - no syntax highlighting.
# ---

alias ls="ls --group-directories-first --color=never --format=single-column --classify=auto"
alias rm="rm --verbose --recursive"
alias mv="mv --interactive --verbose"
alias cp="cp --interactive --verbose --recursive"
alias mkdir="mkdir --verbose --parents"
alias grep="grep --extended-regexp --ignore-case --color=never"

 # no hidden-files by default
find() { command find "$@" -not -path '*/.*'; }

# parsed json in `*js*` variable
bbq() { BABASHKA_PRELOADS='(def *js* (->> *in* slurp json/parse-string))' bb "$@"; }

export EDITOR=vi
export VISUAL=vi

shopt -s globstar
shopt -s checkjobs
shopt -s nocaseglob
shopt -s nocasematch
shopt -s histappend

export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=10000 # default: 500, too smol

export PS1=$'\n\[\033[1m\][\W] > \[\033[0m\]' # prompt :: [newline: more clearly separate command outputs in history], [bold: make prompt visually distinctive from commands/output]

export TERM=xterm
