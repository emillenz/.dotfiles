#!/usr/bin/env bash

#  ---
#  title:  minimal bashrc
#  author: emil lenz
#  email:  emillenz@protonmail.com
#  date:   2024-11-30
#  info:
# - since we use emacs's shell with comint-mode (and tools like magit, dired over git, cd) and thus never work with a shell inside an actual terminal.  we don't need bloated frameworks, plugins, syntax highlighting, prompt-themes etc...  we don't use fancy "CLI" enhancements like eza, z-jump...
# - we NEVER write POSIX shell scripts, we use a proper scripting language (babashka/ruby/...). if you can't do it in a oneliner, you switch!
# - for the times we do work in the terminal, we want a minimalist using shell's coreutils+vi to replace all plugins and "cli-tools".  we make it a B&W experience, no syntax highlighting (shell nor vi) no colors for ls/find/grep.  no tmux, we instead use the shell's job control functionalities (fg, bg, jobs, C-z) and start vi instances in the respective project root directories, then use ~:sh~ within vi to access that shell (or even create subjobs just for that project (eg. manpage).  use {less, grep, vi-buffer} to read/search/filter command output.
#  ---

export GOPATH="$HOME/.local/share/go"

export PATH="$PATH:$GOPATH"
export PATH="$PATH:$HOME/.config/emacs/bin"
export PATH="$PATH:$HOME/.cargo/bin"

export BROWSER="firefox"
export EDITOR="vi"
export VISUAL="vi"
alias vi="vi -u ~/.vimrc" # HACK :: using vim-minimal => must explicitly source vimrc file

alias ls="ls --human-readable --group-directories-first --color=never"
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
