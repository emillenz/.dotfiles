# ---
# title:  fish shell config
# author: emil lenz
# email:  emillenz@protonmail.com
# date:   2024-05-04
# info:
# - favor '--long-flags' over '-f' in order to make the code more unambiguous (shortflags have different meaning across different programs), readeable, easier to maintain (and you'll remember flags better for a program (and don't have to go look them up everytime)).
# - tipp :: for prototyping (long) commands, move from the terminal to a emacs buffer (with: #!) then prototype the command until the output is satisfactory (using eval-buffer/expression to immediately see the result and correct the command).
# ---

# OPTIONS
set -g fish_cursor_replace_one underscore
set -g fish_greeting ''
fish_config theme choose modus_operandi

# PATH
set -gx PATH $PATH ~/.config/{bin, emacs/bin} ~/.cargo/bin ~/.local/bin ~/.local/share/gem/ruby/3.3.0/bin

# PROGRAMS
set -gx EDITOR emacsclient --alternate-editor emacs
set -gx VISUAL emacsclient -nw --alternate-editor emacs
set -gx BROWSER firefox

# PAGER
alias cat bat
set -gx PAGER bat --paging=always
set -gx MANPAGER bat --paging=always
set -gx MANWIDTH 100

# ALIASES :: better defaults
alias ls "ls -v --human-readable --group-directories-first --color=auto"
alias rm "rm --recursive --verbose"
alias du "du --human-readable"
alias mv "mv --verbose"
alias cp "cp --recursive --verbose"
alias yay "yay --noconfirm"
alias echo "echo -e"
alias curl "curl --silent"
alias sed "sed --regexp-extended" # consistent regex-syntax with emacs, rg, fd, ...
alias irb "irb --readline"
# alias grep rg
# alias find fd

# FZF
fzf_configure_bindings --directory=\cf --history --git_log --git_status --variables --processes # NOTE :: disable useless (history already inbuilt in fish: /)
set -gx FZF_DEFAULT_OPTS --reverse --height=16 --color light --scheme path

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
# HACK :: must produce no output => fish prompt displays mode instead
function fish_mode_prompt
end

function fish_prompt
    # newlines to clearly separate commands in history (+ allows to jump to begin/end of command in tmux using: '{}')
    set -l last_status $status # NOTE :: must be first statement
    set -l stat
    if test $last_status -ne 0
        set stat (set_color $fish_color_error)"[$last_status]"
    end

    set -l dir (set_color $fish_color_cwd)(prompt_pwd --full-length-dirs=4)

    set -l prompt $(
        switch $fish_bind_mode
            case insert
                printf "$(set_color --bold purple)>"
            case '*'
                printf "$(set_color --bold blue)|"
        end
    )

    set -l fish_color_line_bg '#dae5ec' # modus theme

    echo "\n$(set_color --background=$fish_color_line_bg --bold) $dir $stat $prompt $(set_color normal)"
end

function open_emacs_dwim --description "open editor with args, if nil open editor with fileexplorer in current directory"
    if test -z "$argv"
        emacs -nw (pwd)
    else
        emacs -nw $argv
    end
end
alias e open_emacs_dwim

function mkdir_cd --description "create a directory and set cwd"
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
