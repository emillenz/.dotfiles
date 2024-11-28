# ---
# title:  fish shell config
# author: emil lenz
# email:  emillenz@protonmail.com
# date:   2024-05-04
# info:
# - using `u_` prefix to indicate user defined functions (defined in `./functions`)
# ---

# OPTIONS
set -g fish_cursor_replace_one underscore
set -g fish_cursor_insert line # indicate mode change clearly
set -g fish_greeting ''
fish_config theme choose modus_operandi # ./themes/modus_operandi.theme

# PATH
set -gx PATH $PATH ~/.config/{bin, emacs/bin} ~/.cargo/bin ~/.local/bin ~/.local/share/gem/ruby/3.3.0/bin

# PROGRAMS
set -gx EDITOR emacsclient -nw --alternate-editor='emacs -nw'
set -gx VISUAL emacsclient --reuse-frame --alternate-editor=emacs
set -gx BROWSER firefox

# PAGER
alias cat bat
set -gx PAGER bat --paging=always
set -gx MANPAGER bat --paging=always
set -gx MANWIDTH 100

# ALIASES :: better defaults
alias ls 'ls -v --human-readable --group-directories-first --color=auto'
alias rm 'rm --recursive --verbose'
alias du 'du --human-readable'
alias mv 'mv --verbose'
alias cp 'cp --recursive --verbose'
alias yay 'yay --noconfirm'
alias curl 'curl --silent'
alias echo 'echo -e'
alias e u_editor

# FZF
fzf_configure_bindings --directory=\cf --history --git_log --git_status --variables --processes # NOTE :: disable useless (history already inbuilt in fish: /)
set -gx FZF_DEFAULT_OPTS --reverse --height=16 --color light --scheme=path

# VIM KEYBINDINGS
set -g fish_key_bindings fish_vi_key_bindings
bind --mode=normal K __fish_man_page # consistent with vim: K - view documentation
bind --mode=normal V __fish_preview_current_file
bind --mode=insert \t accept-autosuggestion
bind --mode=insert \cn complete # consistent with vim: C-{n/p} - completion next/prev
bind --mode=insert \cp up-line # consistent with vim: C-{n/p} - completion next/prev

# FUNCTIONS
function fish_mode_prompt
    # set to nil, since we indicate mode using the cursor-style.
end

function fish_prompt --description="newlines to clearly separate commands in history (+ allows to jump to begin/end of command in tmux using: {}).
    using modus theme colors configured by the terminal.
"
    set -l last_exit_code $status # NOTE :: must be first statement
    set -l last_exit_status (
        if test $last_exit_code -ne 0
            echo -n (set_color $fish_color_error)"[$last_exit_code]"
        end
    )
    set -l dir (set_color $fish_color_cwd)(prompt_pwd --full-length-dirs 4)
    set -l prompt "$(set_color brblue)>$(set_color black)"

    set -l git_branch (
        if git branch &>/dev/null
            echo -n $(set_color bryellow)"[$(git branch --color=always --show-current)]"
            else
                echo -n ""
        end

    )

    set -l fish_color_line_bg '#dae5ec' # modus theme active bg (os global accent color)
    echo -e "\n $(set_color --background=$fish_color_line_bg --bold) $git_branch $dir $last_exit_status $prompt $(set_color normal) "
end

