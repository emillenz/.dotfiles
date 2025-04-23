" ---
" title: minimalist vim config (no plugins, no vimscript)
" author: emil lenz
" email: emilllenz@protonmail.com
" date: [2024-12-14]
" ---

set nocompatible
set hidden
set showcmd
set autoindent
set ignorecase
set smartcase
set incsearch
set nostartofline
set smarttab
set smartindent
set confirm
set shortmess=a
set gdefault
set autoread
set relativenumber
set number
set noruler
set noshowmode
set laststatus=0
set fillchars=eob:\ ,lastline:\ ,
set nobackup
set undofile
set undodir=~/.vim//,/tmp//
set directory=~/.vim//,/tmp//
set completeopt=
set path=.,,**/*
set wildignore=*.o,.*,.a,.so
set wildmenu
set wildignorecase
set wildoptions="fuzzy,wild,pum"
set wildchar=<c-@>
set wildmode=longest:full
set ttimeout
set ttimeoutlen=50
set formatoptions+=jn
set iskeyword+=-
set wrap
set breakindent
set linebreak
set shellcmdflag=-lc
set shiftround
set hlsearch
set splitbelow
set encoding=utf8

set notermguicolors
set background=light
set nocursorline
syntax off
filetype plugin indent on

let g:netrw_banner=0
let g:netrw_hide = 1
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+'
let g:netrw_localcopydircmd = 'cp --recursive'
let g:netrw_cursor = 5
autocmd FileType netrw nmap <buffer> h -^ | nmap <buffer> l <cr> |
nmap <buffer> e %
command! R Rexplore

autocmd BufWritePre * :silent %s/\s\+$//e

nnoremap gf gF
nnoremap Y y$
nnoremap _ "_d
nnoremap L i<cr><esc>
inoremap {<CR> {<CR>}<Esc>O
nnoremap <silent> <esc> <esc>:nohl<cr>
nnoremap <expr><silent> ' feedkeys('`' . toupper(nr2char(getchar(-1, {'cursor': 'keep'}))) . '`"zz', 'nt')
cnoreabbrev term term++curwin

cnoreabbrev <silent> cw cw \| silent only
autocmd FileType qf nnoremap <buffer><silent> <cr> <cr>:wincmd o<cr>
nnoremap <silent> ]q :silent cnext<cr>
nnoremap <silent> [q :silent cprevious<cr>
nnoremap <silent> [Q :silent cfirst<cr>
nnoremap <silent> ]Q :silent clast<cr>

onoremap { V{
onoremap } V}

nnoremap p ]p
nnoremap P [p

map [[ ?{<CR>w99[{
map ][ /}<CR>b99]}
map ]] j0[[%/{<CR>
map [] k$][%?}<CR>

nnoremap Q @q
cnoremap @ normal @

nnoremap go mqo<esc>`q
nnoremap gO mqO<esc>`q

nnoremap v <nop>
nnoremap V <nop>
nnoremap s <nop>
nnoremap S <nop>
nnoremap <c-w> <nop>
nnoremap <c-e> <nop>
nnoremap <c-y> <nop>

nnoremap <c-d> <c-d>zz
nnoremap <c-u> <c-u>zz
nnoremap <c-o> <c-o>zz
nnoremap <c-i> <c-i>zz

nnoremap s mqv
nnoremap ss mq^v$
vnoremap ( <esc>`>a)<esc>`<i(<esc>`q
vnoremap [ <esc>`>a]<esc>`<i[<esc>`q
vnoremap { <esc>`>a}<esc>`<i{<esc>`q
vnoremap " <esc>`>a"<esc>`<i"<esc>`q
vnoremap ' <esc>`>a'<esc>`<i'<esc>`q
nnoremap ds( mqva(<esc>`>"_x`<"_x`q
nnoremap ds[ mqva[<esc>`>"_x`<"_x`q
nnoremap ds{ mqva{<esc>`>"_x`<"_x`q
nnoremap ds" mqva"<esc>`>"_x`<"_x`q
nnoremap ds' mqva'<esc>`>"_x`<"_x`q
