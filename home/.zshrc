#!/usr/bin/env zsh

#  ---
#  title:  minimal zshrc
#  author: emil lenz
#  email:  emillenz@protonmail.com
#  date:   2024-11-30
#  info:

# since we use emacs's shell with comint-mode (and tools like magit, dired over
# git, cd) and thus don't run zsh inside an actual terminal.  we don't use/need
# bloated framworks or plugins.

# we NEVER write shell-script, instead we switch over to writing/prototyping in
# ruby (or python if it must be portable).  as soon as you need to use
# programming constructs or sed/awk..., it's time to switch.

#  ---

export PATH="$PATH:$HOME/.config/emacs/bin"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/.local/bin"

export GEM_HOME="$HOME/.local/share/gem"
export PATH="$PATH:$GEM_HOME/bin"

export EDITOR="emacsclient -nw --alternate-editor='emacs -nw'"
export VISUAL="emacsclient --reuse-frame --alternate-editor=emacs"
export BROWSER="firefox"
export TERMINAL="alacritty"

alias -g ls="ls -v --human-readable --group-directories-first --color=auto"
alias -g rm="rm --recursive --verbose"
alias -g du="du --human-readable"
alias -g mv="mv --verbose"
alias -g cp="cp --recursive --verbose"
alias -g dnf="sudo dnf --assumeyes"
alias -g curl="curl --silent"
alias -g echo="echo -e"

setopt NO_CASE_GLOB
setopt AUTO_CD
setopt SHARE_HISTORY
setopt APPEND_HISTORY
setopt HIST_IGNORE_DUPS

# minimal, functional prompt, (no visual sugar.  same as emacs).
# using empty line to clearly separate commands in history.
PROMPT=$'\n%B%F{cyan}%(?..[%?]) %~ > %b%f'

# for when you must exit emacs
bindkey -v
