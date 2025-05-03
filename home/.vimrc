" ---
" title: minimalist vim config (no plugins, no vimscripting)
" author: emil lenz
" email: emilllenz@protonmail.com
" date: [2024-12-14]
" ---

set formatoptions+=n
set textwidth=0
set wrapmargin=0
set cpoptions+=JM
set iskeyword+=-
set shellcmdflag=-lc
set encoding=utf8
set nocompatible
set hidden
set nostartofline
set confirm
set autoread
set ttimeout
set ttimeoutlen=50
set fillchars=eob:\ ,lastline:\ ,
set history=10000
set cmdwinheight=1
set showcmd
set shortmess+=a

set autoindent
set smarttab
set smartindent
set shiftround
set noexpandtab
set shiftwidth=8
filetype plugin indent on

set nobackup
set undofile
set viminfofile=~/.vim/.viminfo
set undodir=~/.vim
set directory=~/.vim//,/tmp//

set path=.,,**/*
set wildignore=*.o,.a,.so,.*
set wildmenu
set wildignorecase
set wildchar=<c-i>
set wildmode=longest:full
set wildoptions=tagfile,pum
set pumheight=8
set completeopt=

set wrap
set breakindent
set linebreak

set relativenumber
set number
set noruler
set noshowmode
set showtabline=0
set nocursorline
set laststatus=0

syntax off
set notermguicolors
set background=light
highlight Pmenu ctermbg=white
highlight PmenuSel ctermbg=grey
highlight LineNr ctermfg=grey

set gdefault
set incsearch
set hlsearch
set ignorecase
set smartcase

runtime! ftplugin/man.vim

nnoremap gf gF
nnoremap Y y$
nnoremap _ "_d
nnoremap L i<cr><esc>
inoremap {<cr> {<cr>}<esc>O
nnoremap <silent> <esc> :nohlsearch<cr>
nnoremap <silent> & :&<cr>

nnoremap v <nop>
nnoremap V <nop>
nnoremap H <nop>
nnoremap M <nop>
nnoremap <c-w> <nop>
nnoremap <c-e> <nop>
nnoremap <c-y> <nop>

onoremap } V}
onoremap { V{

nnoremap p ]p
nnoremap P [p

nnoremap Q @q
cnoremap @ normal @

nnoremap go mqo<esc>`q
nnoremap gO mqO<esc>`q

map [[ ?{<CR>w99[{
map ][ /}<CR>b99]}
map ]] j0[[%/{<CR>
map [] k$][%?}<CR>

nnoremap s <nop>
nnoremap S <nop>
nnoremap s mqv
nnoremap ss mq^v$
vnoremap ( <esc>`>a)<esc>`<i(<esc>`q
vnoremap { <esc>`>a}<esc>`<i{<esc>`q
vnoremap " <esc>`>a"<esc>`<i"<esc>`q
vnoremap ' <esc>`>a'<esc>`<i'<esc>`q
vnoremap ` <esc>`>a`<esc>`<i`<esc>`q
nnoremap ds( mqva(<esc>`>"_x`<"_x`q
nnoremap ds[ mqva[<esc>`>"_x`<"_x`q
nnoremap ds{ mqva{<esc>`>"_x`<"_x`q
nnoremap ds" mqvi"<esc>`>l"_x`<h"_x`q
nnoremap ds' mqvi'<esc>`>l"_x`<h"_x`q
nnoremap ds` mqvi`<esc>`>l"_x`<h"_x`q

tnoremap <c-w> <nop>
tnoremap <c-w> <c-w>.
tnoremap <c-r> <c-w>"
tnoremap <silent> <c-^> <c-w>:buffer #<cr>
tnoremap <c-\> <nop>
tnoremap <c-\> <c-w>N
nnoremap <silent> <c-\> :if bufexists("!/usr/bin/bash") \| buffer /usr/bin/bash \| else \| execute "term" \| endif<cr>

autocmd BufWritePre * let b:v = winsaveview() | keeppatterns %s/\s\+$//e | call winrestview(b:v)
autocmd ShellCmdPost * silent redraw!
command! Copy call system("xsel --clipboard --input", @")

autocmd BufWinEnter * silent! only
autocmd QuickFixCmdPost * cwindow | only
autocmd FileType qf nmap <buffer> <cr> <cr>zz
cnoreabbrev cw cwindow \| only
cnoreabbrev grep sil grep!
cnoreabbrev make sil make!

let g:netrw_banner = 0
let g:netrw_hide = 1
let g:netrw_list_hide = "\(^\|\s\s\)\zs\.\S\+"
let g:netrw_localcopydircmd = "cp --recursive"
let g:netrw_cursor = 5
autocmd FileType netrw nmap <buffer> h - | nmap <buffer> l <cr>

function! SessionLoad()
	delmarks A-Z
	if filereadable(".vimsession")
		source .vimsession
		rviminfo! .viminfo
	endif
endfunction

function! SessionMake(new)
	if a:new || filereadable(".vimsession")
		mksession! .vimsession
		wviminfo! .viminfo
	endif
endfunction

nnoremap <silent><expr> ' ":buffer " . fnameescape(getpos("'" . toupper(nr2char(getchar(-1, {"cursor": "keep"}))))[0]) . "<cr>"
autocmd VimEnter * call SessionLoad()
autocmd VimLeavePre * call SessionMake(0)
command! Mks call SessionMake(1)
