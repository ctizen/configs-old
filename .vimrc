set nocompatible
set backspace=indent,eol,start

set number
set incsearch
set autoindent
set ruler
set autowrite
set smarttab
set linebreak
set spell
set et
set title

if !has('nvim')
    set ttymouse=xterm2
endif
set mouse=a
set history=50
set tabstop=4
set matchtime=2
set matchpairs+=<:>

syntax enable
filetype plugin indent on
filetype indent on
set sw=4

" Specify a directory for plugins
call plug#begin('~/.vim/plugged')

" -------- status line ---------
Plug 'itchyny/lightline.vim'
set noshowmode

" ---------- git support -------- 
Plug 'tpope/vim-fugitive'
Plug 'tomtom/quickfixsigns_vim'

" -------- tags etc ---------
set statusline+=%{gutentags#statusline()}
let g:gutentags_ctags_exclude = ["*.min.js", "*.min.css", "build", "vendor", ".git", "node_modules", "*.vim/bundle/*"]
Plug 'ludovicchabant/vim-gutentags'

" -------- Buffers and tabs
Plug 'zefei/vim-wintabs'
nnoremap <C-A-Left> :bprev<CR>
nnoremap <C-A-Right> :bnext<CR>
Plug 'moll/vim-bbye'
nnoremap <C-A-Q> :Bdelete<CR>

" ------- NERDTree -------
Plug 'preservim/nerdtree'
nnoremap <F2> :NERDTreeToggle<CR>
nnoremap <C-F2> :NERDTreeFocus<CR>
" Start NERDTree when Vim is started without file arguments.
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists('s:std_in') | NERDTree | endif
" Open the existing NERDTree on each new tab.
autocmd BufWinEnter * silent NERDTreeMirror

" -------- fzf search --------
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
nnoremap <C-S-F> :FZF<CR>

" --------- js support --------
Plug 'pangloss/vim-javascript'
Plug 'neoclide/coc.nvim' , { 'branch' : 'release' }

" -------- ts support ---------
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'Quramy/tsuquyomi'

" ------- color scheme ----------
Plug 'jacoborus/tender.vim'
if (has("termguicolors"))
 set termguicolors
endif
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
colorscheme tender
let g:lightline = { 'colorscheme': 'tender' }

" --------- php support ---------
Plug 'phpactor/phpactor', {'for': 'php', 'tag': '*', 'do': 'composer install --no-dev -o'}
Plug 'tobyS/vmustache'
Plug 'tobyS/pdv'
Plug 'SirVer/ultisnips'
let g:pdv_template_dir = $HOME ."/.vim/plugged/pdv/templates_snip"
nnoremap <C-S-P> :call pdv#DocumentWithSnip()<CR>

" Initialize plugin system
call plug#end()
