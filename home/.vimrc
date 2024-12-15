" ---
" title: minimalist vimrc
" author: emil lenz
" email: emilllenz@protonmail.com
" date: [2024-12-14]
" info:
" - intended for usage with vim-minimal
" - minimalist & bare-bones:
"   - no plugins, no custom bindings :: use macros, registers and shell commands
"   - no syntax highlighting
"   - disable inefficient keybindings :: enforce using motions + relative line jumping
" ---

set nocompatible
set hidden
set relativenumber
set autoindent
set autowriteall
set smartcase
set nobackup
set autoread
set incsearch
set path+=**

nnoremap v <nop>
nnoremap V <nop>
nnoremap } <nop>
nnoremap { <nop>
