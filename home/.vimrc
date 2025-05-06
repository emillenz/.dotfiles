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
set shortmess+=aF
set shortmess-=S
set matchpairs+=<:>

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

set noruler
set noshowcmd
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

for x in ["s", "S", "H", "M", "<c-w>", "<c-e>", "<c-y>", "gu", "gU", "~"]
	execute "nnoremap" x "<nop>"
	execute "vnoremap" x "<nop>"
endfor

autocmd ModeChanged n:[vV\x16] let g:pos = getpos('.')
autocmd ModeChanged [vV\x16]:n call setpos('.', g:pos)

for x in ["d", "c", "y", "=", "<", ">", "gw"]
	execute "nnoremap" x  x . x
endfor
vnoremap < <gvh
vnoremap > >gvl

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

vnoremap <silent><expr> @ ":normal @" . nr2char(getchar(-1, {"cursor": "keep"})) . "\<lt>cr>"
nnoremap Q @q
vnoremap <silent> Q :normal @q<cr>

vnoremap <silent> & :normal &<cr>
nnoremap <silent> & :&<cr>

nnoremap gf gF
nnoremap Y y$
nnoremap L i<cr><esc>
inoremap {<cr> {<cr>}<esc>O
nnoremap <silent> <esc> :nohlsearch<cr>
nnoremap _ "_d

nnoremap x "_x
nnoremap X "_X

nnoremap <silent><expr> j (v:count > 0 ? "m`" . v:count : "") . "j"
nnoremap <silent><expr> k (v:count > 0 ? "m`" . v:count : "") . "k"
vnoremap <silent><expr> j (v:count > 0 ? "m`" . v:count : "") . "j"
vnoremap <silent><expr> k (v:count > 0 ? "m`" . v:count : "") . "k"

for x in ["{", "}", "(", ")"]
	execute "nnoremap <silent>" x ":keepjumps normal!" x . "<cr>"
endfor

nnoremap p ]p
nnoremap P [p
vnoremap p ]p
vnoremap P [p

nnoremap <silent> go :call append(line("."), "")<cr>
nnoremap <silent> gO :call append(line(".") - 1, "")<cr>

nnoremap [q :cprevious<cr>
nnoremap ]q :cnext<cr>
nnoremap [Q :cfirst<cr>
nnoremap ]Q :clast<cr>
nnoremap [<c-Q> :cnfile<cr>
nnoremap ]<c-Q> :cpfile<cr>

tnoremap <c-v> <c-w>""
tnoremap <c-w> <nop>
tnoremap <c-w> <c-w>.
tnoremap <silent> <c-^> <c-w>:buffer #<cr>
tnoremap <c-\> <nop>
tnoremap <c-o> <c-w>N

cnoremap <expr> <c-i> wildmenumode() ? "<c-y><c-i>" : "<c-i>"
cnoremap <c-a> <home>
cnoremap <esc>* <c-a>
cnoremap <c-e> <end>
cnoremap <c-f> <right>
cnoremap <c-d> <del>
cnoremap <c-o> <c-f>
cnoremap <c-b> <left>
cnoremap <esc>f <c-right>
cnoremap <esc>b <c-left>
cnoremap <esc>d <c-right><c-w>
cnoremap <c-k> <c-\>e getcmdpos() == 1 ? "" : getcmdline()[:getcmdpos()-2]<cr>

function! Shell()
	let b:bufname = "shell"
	if bufloaded(b:bufname)
		execute "buffer" b:bufname
	else
		call term_start($SHELL, {"term_name": b:bufname})
	endif
endfunction
command! Shell call Shell()

function! GlobalMarkGoto()
       execute "buffer" fnameescape(getpos("'" . toupper(nr2char(getchar(-1, {"cursor": "keep"}))))[0])
endfunction
nnoremap <silent> ' :call GlobalMarkGoto()<cr>

function! TrimWhitespace()
	let v = winsaveview()
	keeppatterns %s/\s\+$//e
	call winrestview(v)
endfunction
autocmd BufWritePre * call TrimWhitespace()

command! Copy call system("xsel --clipboard --input", @")

autocmd BufWinEnter * silent! only
autocmd QuickFixCmdPost * cwindow | only
autocmd ShellCmdPost * silent redraw!
command! Cwindow cwindow | only
command! -nargs=* Make silent make! <args>
command! -nargs=+ -complete=file_in_path Grep silent grep! <args>
command! -nargs=+ -complete=file_in_path Cprg cgetexpr system("<args>")

let g:sesh_dir = expand("~/.vim/")
function! SeshFile(type)
	return expand("~/.vim/") . substitute(getcwd(), "/", "_", "g") . (a:type ? ".vim" : ".viminfo")
endfunction

function! SeshLoad()
	if filereadable(SeshFile(1))
		delmarks A-Z
		execute "source" SeshFile(1)
		execute "rviminfo!" SeshFile(0)
	endif
endfunction

function! SeshRemove()
	call system("rm " . SeshFile(1) . " " . SeshFile(0))
endfunction
command! SeshRm call SeshRemove()

function! SeshMake(make)
	if a:make || filereadable(SeshFile(1))
		execute "mksession!" SeshFile(1)
		execute "wviminfo!" SeshFile(0)
	endif
endfunction
command! SeshMake call SeshMake(1)

autocmd VimEnter * call SeshLoad()
autocmd VimLeavePre * call SeshMake(0)

filetype plugin indent on
runtime! ftplugin/man.vim

let g:netrw_banner = 0
let g:netrw_list_hide = "\(^\|\s\s\)\zs\.\S\+"
let g:netrw_localcopydircmd = "cp --recursive"
let g:netrw_cursor = 5
let g:netrw_altfile = 1

function! NetrwSetup()
	setlocal bufhidden=wipe
	nmap <buffer> h -^
	nmap <buffer> l <cr>
	nmap <buffer> <tab> mfj
	nmap <buffer> e %
endfunction
autocmd FileType netrw call NetrwSetup()
