#!/usr/bin/env bash

# ---
# title:  minimalist bashrc
# author: emil lenz
# email:  emillenz@protonmail.com
# date:   2024-11-30
# info:
#   - case insesitive :: configured if available (grep, find...)
#   - no colors, no syntax hilighting :: minimalist (no colorful fruit-salad)
#   - no "rewritten in rust" tools that add colours to everything.  we use the minimalist, readily available GNU coreutils (no unneccessary dependecies).  we use of aliases & functions for more ergonomic default behaviour.
# ---

alias ls="ls --group-directories-first --format=single-column --classify=auto --color=never"
alias grep="grep --extended-regexp --ignore-case --color=never"
alias rm="rm --verbose --recursive --interactive=once" # require confirmation (can't be undone)
alias mkdir="mkdir --verbose --parents"
alias bb-js="BABASHKA_PRELOADS='(def *js* (->> *in* slurp json/parse-string))' bb" # parsed json in `*js*` variable
alias bb="rlwrap bb"

function mv { mkdir "$(dirname "${@: -1}")"; command mv --interactive --verbose "$@"; } # create neccessary target dirs
function cp { mkdir "$(dirname "${@: -1}")"; command cp --interactive --verbose --recursive "$@"; } # create neccessary target dirs

function find { command find "$@" -not -path '*/.*'; }  # better defaults: no dirs, no dotfiles

export EDITOR=vi
export VISUAL=vi

shopt -s globstar
shopt -s checkjobs
shopt -s nocaseglob
shopt -s nocasematch
shopt -s histappend

export HISTCONTROL=ignoredups:erasedups # no duplicates
export HISTSIZE=10000 # default: 500, too smol

export PS1=$'\n\[\033[1m\][%\j] [\W] > \[\033[0m\]' # prompt :: [newline: more clearly separate command outputs in history], [bold: make prompt visually distinctive from commands/output]

export TERM=xterm
