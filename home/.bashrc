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

export PS1='\n\[\033[1m\007\][\W] \[\033[0m\007\]' # `[1m` :: bold
export PROMPT_COMMAND='printf "\033]133;A\007"' # `]133;A` :: enable {next,prev}-prompt navigation in tmux/vim.

alias\
	ls="ls --no-group --human-readable --group-directories-first --time-style=long-iso --file-type --format=single-column --color=never -F"\
	grep="grep --line-number --ignore-case"\
	rm="rm --verbose --recursive"\
	less="less --ignore-case"\
	cp="cp --verbose --recursive"\
	mkdir="mkdir --verbose --parents"\
	pgrep="pgrep --ignore-case"\
	jb="jobs"\
	less="less --ignore-case"

export\
	EDITOR="vim"\
	VISUAL="vim"\
	HISTCONTROL=ignoreboth:erasedups\
	HISTSIZE=1000\
	PYTHON_BASIC_REPL=1

function find { command find $@ -type f -not -path './.*'; }
function rjs { ruby -rjson -e '$js = JSON.parse(ARGF.read);' $@; }

stty -ixon
