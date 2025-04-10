#!/usr/bin/env bash

# ---
# title:  bash shell config
# author: emil lenz
# email:  emillenz@protonmail.com
# date:   2024-11-30
# ---

alias\
	ls="ls --group-directories-first --format=single-column --file-type --color=never"\
	grep="grep --extended-regexp --ignore-case"\
	rm="rm --verbose --recursive --interactive=once"\
	cp="cp --verbose --recursive"\
	mkdir="mkdir --verbose --parents"\
	pgrep="pgrep --ignore-case"\
	less="less --ignore-case"

# no directories, no hidden files
# usage: `find [<pattern>] [<dirs>]*
function find { command find "${@:2:$#}" -iname "*$1*" -not -path './.*'; }

function rjs { ruby -rjson -e 'in = JSON.parse(ARGF.read);'"$@" ; }

export\
	EDITOR="vi"\
	VISUAL="vi"\
	HISTCONTROL=ignoredups:erasedups\
	HISTSIZE=10000

shopt -s\
	globstar\
	checkjobs\
	nocaseglob\
	nocasematch\
	histappend\
	patsub_replacement\
	autocd

# newline (\n) :: clearly separate command output from next prompt in history.
# bold (\[\e]133;A\e\\\]) :: make prompt visually distinctive from the command & it's output.
# tmux (\[\e[1m\]) :: needed to enable {next,prev}-prompt navigation.
export PS1=$'\n\[\e]133;A\\\]\[\e[1m\][\W] > \[\e[0m\]'
