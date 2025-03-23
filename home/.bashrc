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
alias rm="rm --verbose --recursive --interactive=once"
alias mkdir="mkdir --verbose --parents"
alias bb-js="BABASHKA_PRELOADS='(def *js* (->> *in* slurp json/parse-string))' bb" # parsed json in `*js*` variable
alias bb="rlwrap bb"
alias pgrep="pgrep --ignore-case"

function find { command find "${@: 2}" -iname "*$1*" -not -path './.*'; } # no directories, no hidden files, usage: `find <pattern> [<dir>...]

export EDITOR="vi"
export VISUAL="vi"

shopt -s globstar
shopt -s checkjobs
shopt -s nocaseglob
shopt -s nocasematch
shopt -s histappend

export HISTCONTROL=ignoredups:erasedups # no duplicates
export HISTSIZE=10000 # default: 500, too smol

export PS1=$'\n\[\033[1m\] [\W] > \[\033[0m\]' # prompt :: newline: more clearly separate command outputs in history, bold: make prompt visually distinctive from commands/output
PS0="\e[2 q"

export TERM=xterm
