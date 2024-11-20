# ---
# title:  fish shell config
# author: emil lenz
# email:  emillenz@protonmail.com
# date:   2024-05-04
# info:
#  long flags :: favor '--long-flags' over '-f' in order to make the code more unambiguous (shortflags have
#  different meaning across different programs), readeable and easier to maintain.
# ---

# OPTIONS
set -g fish_cursor_replace_one underscore
set -g fish_cursor_insert line # indicate mode change clearly
set -g fish_greeting ''
fish_config theme choose modus_operandi # ./themes/modus_operandi.theme

# PATH
set -gx PATH $PATH ~/.config/{bin, emacs/bin} ~/.cargo/bin ~/.local/bin ~/.local/share/gem/ruby/3.3.0/bin

# PROGRAMS
set -gx EDITOR emacsclient -nw --alternate-editor 'emacs -nw'
set -gx VISUAL emacsclient --create-frame --alternate-editor emacs
set -gx BROWSER firefox

# PAGER
alias cat bat
set -gx PAGER bat --paging always
set -gx MANPAGER bat --paging always
set -gx MANWIDTH 100

# ALIASES :: better defaults
alias ls 'ls -lv --human-readable --group-directories-first --color auto'
alias rm 'rm --recursive --verbose'
alias du 'du --human-readable'
alias mv 'mv --verbose'
alias cp 'cp --recursive --verbose'
alias yay 'yay --noconfirm'
alias curl 'curl --silent'
alias sed 'sed --regexp-extended' # consistent regex-syntax with emacs, rg, fd, ...
abbr ps 'ps -e -o user,pid,time,%cpu,%mem,command --sort %cpu' # get overview
abbr echo printf # get in the habit of using more powerful printf instead...

# FZF
fzf_configure_bindings --directory \cf --history --git_log --git_status --variables --processes # NOTE :: disable useless (history already inbuilt in fish: /)
set -gx FZF_DEFAULT_OPTS --reverse --height 16 --color light --scheme path

# VIM KEYBINDINGS
set -g fish_key_bindings fish_vi_key_bindings
bind --mode normal K __fish_man_page # consistent with vim: K - view documentation
bind --mode insert \t accept-autosuggestion
bind --mode insert \cn complete # consistent with vim: C-{n/p} - completion next/prev
bind --mode insert \cp up-line # consistent with vim: C-{n/p} - completion next/prev
bind --mode normal V __fish_preview_current_file
bind --mode normal V __fish_preview_current_file
bind --mode normal z clear-screen # consistent with vim: zz - center-screen
# bind --mode insert \ce edit_command_buffer # $VISUAL as editor.  but if the command is so complex that you need your editor, you should move to using ruby and a script buffer.

# FUNCTIONS
function fish_mode_prompt
    # set to nil, since we indicate mode using the cursor.
end

function fish_prompt --description 'newlines to clearly separate commands in history (+ allows to jump to begin/end of command in tmux using: {})'
    set -l last_exit_code $status # NOTE :: must be first statement
    set -l last_exit_status $(
        if test $last_exit_code -ne 0
            printf (set_color $fish_color_error)"[$last_exit_code]"
        end
    )
    set -l dir (set_color $fish_color_cwd)(prompt_pwd --full-length-dirs 4)
    set -l prompt "$(set_color brblue)>"
    set -l fish_color_line_bg '#dae5ec' # modus theme
    printf "\n $(set_color --background $fish_color_line_bg --bold) $dir $last_exit_status $prompt $(set_color normal) "
end

function open_editor_dwim --description "open editor with args, if nil open editor with fileexplorer in current directory"
    if test -z "$argv"
        $EDITOR
    else
        $EDITOR $argv
    end
end
alias e open_editor_dwim

function mkdir_cd --description 'create a directory and set cwd'
    command mkdir $argv
    if test $status = 0
        switch $argv[(count $argv)]
            case '-*'
                #
            case '*'
                cd $argv[(count $argv)]
                return
        end
    end
end
alias mkdir mkdir_cd
