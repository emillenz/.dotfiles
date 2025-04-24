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
set shortmess+=a
set gdefault
set autoread
set relativenumber
set number
set noruler
set noshowmode
set laststatus=0
set showtabline=0
set fillchars=eob:\ ,lastline:\ ,
set nobackup
set undofile
set undodir=~/.vim//,/tmp//
set directory=~/.vim//,/tmp//
set path=.,,**/*
set wildignore=*.o,.*,.a,.so
set wildmenu
set wildignorecase
set wildchar=<c-@>
set wildmode=longest:full
set wildoptions=tagfile,pum
set pumheight=8
set completeopt=
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
set cmdwinheight=1

set notermguicolors
set background=light
set nocursorline
syntax off
filetype plugin indent on

let g:netrw_banner = 0
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+'
let g:netrw_localcopydircmd = 'cp --recursive'
let g:netrw_cursor = 5
let g:netrw_altfile = 1
autocmd FileType netrw nmap <buffer> h - | nmap <buffer> l <cr>

autocmd BufWritePre * :silent %s/\s\+$//e

nnoremap gf gF
nnoremap Y y$
nnoremap _ "_d
nnoremap L i<cr><esc>
inoremap {<CR> {<CR>}<Esc>O
nnoremap <silent> <esc> <esc>:nohl<cr>

nnoremap <silent> ' :execute 'buffer ' .  fnameescape(getpos("'" . toupper(nr2char(getchar(-1, {'cursor': 'keep'}))))[0])<cr>

cnoreabbrev term term++curwin

autocmd FileType qf nnoremap <buffer><silent> <cr> <cr>:wincmd o<cr>
cnoreabbrev <silent> cw cw \| silent only
cnoreabbrev <silent> lw lw \| silent only
autocmd QuickFixCmdPost [^l]* call feedkeys(':cw\<cr>')
autocmd QuickFixCmdPost l* call feedkeys(':lw\<cr>')
nnoremap <silent> ]q :cnext<cr>
nnoremap <silent> [q :cprevious<cr>
nnoremap <silent> [Q :cfirst<cr>
nnoremap <silent> ]Q :clast<cr>

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
vnoremap ` <esc>`>a`<esc>`<i`<esc>`q
nnoremap ds( mqva(<esc>`>"_x`<"_x`q
nnoremap ds[ mqva[<esc>`>"_x`<"_x`q
nnoremap ds{ mqva{<esc>`>"_x`<"_x`q
nnoremap ds" mqva"h<esc>`>"_x`<"_x`q
nnoremap ds' mqva'h<esc>`>"_x`<"_x`q
nnoremap ds` mqva`h<esc>`>"_x`<"_x`q
