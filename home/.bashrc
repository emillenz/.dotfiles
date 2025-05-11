#!/usr/bin/env bash

# ---
# title:  bash shell config
# author: emil lenz
# email:  emillenz@protonmail.com
# date:   [2024-11-30]
# ---

export IFS=$'\n'

shopt -s\
      globstar\
      checkjobs\
      nocaseglob\
      nocasematch\
      histappend\
      patsub_replacement\
      expand_aliases\
      autocd

alias\
    grep="grep --ignore-case"\
    rm="rm --verbose --recursive"\
    cp="cp --verbose --recursive"\
    mkdir="mkdir --verbose --parents"\
    pgrep="pgrep --ignore-case"

export\
	EDITOR="emacsclient --reuse-frame --alternate-editor=emacs -nw"\
	VISUAL="emacsclient --reuse-frame --alternate-editor=emacs -nw"\
	HISTCONTROL=ignoreboth:erasedups\
	HISTSIZE=1000

export PS1='\n\[\e[1m\][\W] \[\e[0m\]'

function find { command find $@ -type f -not -path './.*'; }
function rjs { ruby -rjson -e '$js = JSON.parse(ARGF.read);' $@; }
