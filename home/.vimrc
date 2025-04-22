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
set background=light

set cursorline
colorscheme shine
syntax off
filetype plugin indent on
runtime ftplugin/man.vim

autocmd BufWritePre * :silent %s/\s\+$//e


let g:netrw_banner=0
let g:netrw_keepdir=0
let g:netrw_hide = 1
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+'
let g:netrw_localcopydircmd = 'cp --recursive'
autocmd FileType netrw nmap <buffer> h -^ | nmap <buffer> l <cr>

nnoremap gf gF
nnoremap Y y$
nnoremap _ "_d
nnoremap \ i<cr><esc>
nnoremap <silent> <esc> <esc>:nohl<cr>

autocmd FileType qf nnoremap <buffer><silent> <cr> <cr>:wincmd p<cr> | nmap <buffer><silent> j j<cr> | nmap <buffer><silent> k k<cr>
nnoremap ]q :cnext<cr>
nnoremap [q :cprevious<cr>
nnoremap ]Q :crewind<cr>
nnoremap [Q :clast<cr>

nnoremap <c-w> <c-w><c-w>
nnoremap <c-q> <c-w><c-q>
cnoreabbrev term term ++curwin

inoremap {<CR> {<CR>}<Esc>O

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

nnoremap <silent> ' :execute "normal! '" . toupper(nr2char(getchar())) . '`"'<cr>
