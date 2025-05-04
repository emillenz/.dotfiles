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
set shortmess+=aF
set shortmess-=S

set autoindent
set smarttab
set smartindent
set shiftround
set noexpandtab
set shiftwidth=8

set nobackup
set undofile
set viminfofile=~/.vim/.viminfo
set undodir=~/.vim
set directory=~/.vim//,/tmp//

set path=.,,**/*
set wildignore=*.o,.a,.so,.*
set wildmenu
set wildignorecase
set wildoptions=tagfile,pum
set pumheight=8
set wildchar=<c-i>
set wildcharm=<c-i>
set wildmode=longest:full
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
set guicursor=
set background=light
highlight Pmenu ctermbg=white
highlight PmenuSel ctermbg=grey
highlight LineNr ctermfg=grey

set gdefault
set incsearch
set hlsearch
set ignorecase
set smartcase

filetype plugin indent on

nnoremap v <nop>
nnoremap V <nop>
nnoremap s <nop>
nnoremap S <nop>
nnoremap H <nop>
nnoremap M <nop>
nnoremap <c-w> <nop>
nnoremap <c-e> <nop>
nnoremap <c-y> <nop>

nnoremap gf gF
nnoremap Y y$
nnoremap _ "_d
nnoremap L i<cr><esc>
inoremap {<cr> {<cr>}<esc>O
nnoremap <silent> <esc> :nohlsearch<cr>
nnoremap <silent> & :&<cr>
nnoremap Q @q
cnoremap <expr> <c-y> wildmenumode() ? '<c-y><c-i>' : '<c-y>'

onoremap } V}
onoremap { V{

nnoremap p ]p
nnoremap P [p

nnoremap go mqo<esc>`q
nnoremap gO mqO<esc>`q

nnoremap [q :cprevious<cr>
nnoremap ]q :cnext<cr>
nnoremap [Q :cfirst<cr>
nnoremap ]Q :clast<cr>
nnoremap [<c-Q> :cnfile<cr>
nnoremap ]<c-Q> :cpfile<cr>

function! Shell()
	let b:bufname = 'shell'
	if bufloaded(b:bufname)
		execute 'buffer ' . b:bufname
	else
		call term_start($SHELL, {'term_name': b:bufname})
	endif
endfunction
command! Shell call Shell()
tnoremap <c-w> <nop>
tnoremap <c-w> <c-w>.
tnoremap <c-r> <c-w>"
tnoremap <silent> <c-^> <c-w>:buffer #<cr>
tnoremap <c-\> <nop>
tnoremap <c-\> <c-w>N

nnoremap <silent> ' :execute 'buffer ' . fnameescape(getpos("'" . toupper(nr2char(getchar(-1, {'cursor': 'keep'}))))[0])<cr>

autocmd BufWritePre * let b:v = winsaveview() | keeppatterns %s/\s\+$//e | call winrestview(b:v)
command! Copy call system('xsel --clipboard --input', @")

autocmd BufWinEnter * silent! only
autocmd QuickFixCmdPost * cwindow | only
autocmd ShellCmdPost * silent redraw!
command! Cwindow cwindow | only
command! -nargs=* Make silent make! <args>
command! -nargs=+ Grep silent grep! <args>

let g:sesh_dir = expand('~/.vim/')
function! SeshFile(type)
	return g:sesh_dir . substitute(getcwd(), '/', '_', 'g') . (a:type ? '.vim' : '.viminfo')
endfunction

function! SeshLoad()
	if filereadable(SeshFile(1))
		delmarks A-Z
		execute 'source ' . SeshFile(1)
		execute 'rviminfo! ' . SeshFile(0)
		echo '[sesh] loaded'
	endif
endfunction

function! SeshRemove()
	call system('rm ' . SeshFile(1) . ' ' . SeshFile(0))
	echo '[sesh] removed'
endfunction
command! SeshRm call SeshRemove()

function! SeshMake(make)
	if a:make || filereadable(SeshFile(1))
		execute 'mksession! ' . SeshFile(1)
		execute 'wviminfo! ' . SeshFile(0)
	endif
endfunction
command! SeshMake call SeshMake(1)

autocmd VimEnter * call SeshLoad()
autocmd VimLeavePre * call SeshMake(0)

let g:loaded_netrwPlugin = 1
runtime! ftplugin/man.vim
