" ---
" title: minimalist vim config (no plugins)
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

let s:nop_bindings = ["v", "V", "<c-v>", "s", "S", "H", "M", "L", "v", "V", "<c-w>", "<c-e>", "<c-y>"]
for o in s:nop_bindings
	execute 'nnoremap' o '<nop>'
endfor

nnoremap <space> mvv
vnoremap <space> V
nnoremap <c-@> mv<c-v>
vnoremap <c-@> <c-v>
vnoremap <esc> <esc>`v
let s:operators = ['d', 'y', 'c', '=', 'gw', 'gq', 'g~', 'gu', 'gU']
for o in s:operators
	execute 'nnoremap' o o[0] == 'g' ? o[0] . o[1] . o[1] :  o . o
	execute 'vnoremap' o o . '`v'
endfor
nnoremap > >>^
nnoremap < <<^
vnoremap < <gv^
vnoremap > >gv^

vnoremap Q :normal @q<cr>
vnoremap @ :normal @
nnoremap Q @q

nnoremap gf gF
nnoremap Y y$
nnoremap L i<cr><esc>
inoremap {<cr> {<cr>}<esc>O
nnoremap <silent> <esc> :nohlsearch<cr>
nnoremap <silent> & :&<cr>
cnoremap <expr> <c-i> wildmenumode() ? '<c-y><c-i>' : '<c-i>'

" *, # search for selection in visual-mode
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gVzv:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gVzv:call setreg('"', old_reg, old_regtype)<CR>

nnoremap p ]p
nnoremap P [p
vnoremap p ]p
vnoremap P [p

nnoremap go mvo<esc>`v
nnoremap gO mvO<esc>`v

nnoremap [q :cprevious<cr>
nnoremap ]q :cnext<cr>
nnoremap [Q :cfirst<cr>
nnoremap ]Q :clast<cr>
nnoremap [<c-Q> :cnfile<cr>
nnoremap ]<c-Q> :cpfile<cr>

tnoremap <c-w> <nop>
tnoremap <c-w> <c-w>.
tnoremap <c-v> <c-w>""
tnoremap <silent> <c-^> <c-w>:buffer #<cr>
tnoremap <c-\> <nop>
tnoremap <c-\> <c-w>N

function! Shell()
	let bufname = 'shell'
	if bufloaded(bufname)
		execute 'buffer' bufname
	else
		call term_start($SHELL, {'term_name': bufname})
	endif
endfunction
command! Shell call Shell()

function! GlobalMarkGoto()
	execute 'buffer' fnameescape(getpos("'" . toupper(nr2char(getchar(-1, {'cursor': 'keep'}))))[0])
endfunction
nnoremap <silent> ' :call GlobalMarkGoto()<cr>

function! TrimWhitespace()
	let v = winsaveview()
	keeppatterns %s/\s\+$//e
	call winrestview(v)
endfunction
autocmd BufWritePre * call TrimWhitespace()
command! Copy call system('xsel --clipboard --input', @")

autocmd BufWinEnter * silent! only
autocmd QuickFixCmdPost * cwindow | only
autocmd ShellCmdPost * silent redraw!
command! Cwindow cwindow | only
command! -nargs=* Make silent make! <args>
command! -nargs=+ Grep silent grep! <args>

function! SeshFile(type)
	return expand('~/.vim/') . substitute(getcwd(), '/', '_', 'g') . (a:type ? '.vim' : '.viminfo')
endfunction

function! SeshLoad()
	if filereadable(SeshFile(1))
		delmarks A-Z
		execute 'source' SeshFile(1)
		execute 'rviminfo!' SeshFile(0)
	endif
endfunction

function! SeshRemove()
	call system('rm ' . SeshFile(1) . ' ' . SeshFile(0))
endfunction
command! SeshRm call SeshRemove()

function! SeshMake(make)
	if a:make || filereadable(SeshFile(1))
		execute 'mksession!' SeshFile(1)
		execute 'wviminfo!' SeshFile(0)
	endif
endfunction
command! SeshMake call SeshMake(1)

autocmd VimEnter * call SeshLoad()
autocmd VimLeavePre * call SeshMake(0)

let g:loaded_netrwPlugin = 1
runtime! ftplugin/man.vim
