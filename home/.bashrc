#!/usr/bin/env bash

# ---
# title:  bash shell config
# author: emil lenz
# email:  emillenz@protonmail.com
# date:   [2024-11-30]
# ---

export IFS=$'\n' # scripts ignore this value (interactive only)

shopt -s\
	globstar\
	checkjobs\
	nocaseglob\
	nocasematch\
	histappend\
	patsub_replacement\
	expand_aliases\
	autocd

export PS1='\n[\W] '

alias\
	ls="ls --no-group --human-readable --group-directories-first --time-style=long-iso --file-type --format=single-column --color=never -F"\
	grep="grep --ignore-case"\
	rm="rm --verbose --recursive"\
	less="less --ignore-case"\
	cp="cp --verbose --recursive"\
	mkdir="mkdir --verbose --parents"\
	pgrep="pgrep --ignore-case"\
	jb="jobs"\
	less="less --ignore-case"

export\
	EDITOR="emacsclient --reuse-frame --alternate-editor=emacs -nw"\
	VISUAL="emacsclient --reuse-frame --alternate-editor=emacs -nw"\
	HISTCONTROL=ignoreboth:erasedups\
	HISTSIZE=1000

function find { command find $@ -type f -not -path './.*'; }
function rjs { ruby -rjson -e '$js = JSON.parse(ARGF.read);' $@; }
