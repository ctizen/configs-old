set nocompatible

" Load plugins

if filereadable(expand("$HOME/.vim/vundle.vim"))
  source $HOME/.vim/vundle.vim
endif

" General

set t_Co=256

set number                      "Line numbers are good
set backspace=indent,eol,start  "Allow backspace in insert mode
set history=1000                "Store lots of :cmdline history
set showcmd                     "Show incomplete cmds down the bottom
set showmode                    "Show current mode down the bottom
set gcr=a:blinkon0              "Disable cursor blink
set visualbell                  "No sounds
set autoread                    "Reload files changed outside vim
set laststatus=2                "Always display the status line
set hidden                      "Hide buffer instead of closing it
set pastetoggle=<F2>            "Paste without being smart

" Swap file and backups

set noswapfile
set nobackup
set nowb
au FocusLost * :wa

" Persistent undo

if has('persistent_undo')
  silent !mkdir ~/.vim/backups > /dev/null 2>&1
  set undodir=~/.vim/backups
  set undofile
endif

" Indentation

set autoindent
set smartindent
set smarttab
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab

" Enable loading the plugin/indent files for specific file types

filetype plugin indent on

" Wrapping

set nowrap       "Don't wrap lines
set linebreak    "Wrap lines at convenient points

" Folding

set nofoldenable                "don't fold by default
set foldmethod=syntax           "fold based on syntax

" Search

set hlsearch
set incsearch
set ignorecase
set showmatch
set smartcase

" Terminal

set termencoding=utf-8

" Colors

syntax on
set cursorline
set background=dark
" let g:solarized_termcolors=256
colorscheme distinguished
highlight clear SignColumn

" Scrolling

set scrolloff=4
set sidescrolloff=15
set sidescroll=1

" Completion

set wildmenu                "enable ctrl-n and ctrl-p to scroll thru matches
set wildmode=list:longest
set wildignore=*.o,*.so,*.obj,*~ "stuff to ignore when tab completing
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=vendor/rails/**
set wildignore+=vendor/cache/**
set wildignore+=*.gem
set wildignore+=log/**
set wildignore+=tmp/**
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg,*.svg
set wildignore+=*.zip
set wildignore+=*.swp,*.pyc,*.bak,*.class,*.orig
set wildignore+=.git,.hg,.bzr,.svn


" Bindings

" Greping with silver searcher word under cursor
nnoremap <leader>k :silent grep! "\b\s?<C-R><C-W>\b"<CR>:cw<CR>:redr!<CR>

" Fix syntax highlighting issues
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>

" Run node or browser on F5
autocmd BufRead,Bufenter *.js map <F5> :w<Esc>:!node %<CR>
autocmd BufRead,Bufenter *.html map <F5> :exe ':silent !open -a /Applications/Firefox.app %'<CR>:redr!<CR>

" Write with sudo
cmap wi! %!sudo tee > /dev/null %

" Turn off search highlight
nnoremap <leader>a :noh<CR>


" Ctrl-P

let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_regexp = 1

" The Silver Searcher

if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor
  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" Snippets

" UltiSnips completion function that tries to expand a snippet. If there's no
" snippet for expanding, it checks for completion window and if it's
" shown, selects first element. If there's no completion window it tries to
" jump to next placeholder. If there's no placeholder it just returns TAB key

function! g:UltiSnips_Complete()
    call UltiSnips#ExpandSnippet()
    if g:ulti_expand_res == 0
        if pumvisible()
            return "\<C-n>"
        else
            call UltiSnips#JumpForwards()
            if g:ulti_jump_forwards_res == 0
               return "\<TAB>"
            endif
        endif
    endif
    return ""
endfunction

au BufEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"


" Air-line

let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" powerline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''
"
"" Enable the list of buffers
let g:airline#extensions#tabline#enabled = 1

" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'


" Bclose

map <leader>bd :Bclose<CR>


" DelimitMate

let delimitMate_expand_cr = 1


" Bufferegator

nnoremap <leader>h :BuffergatorMruCyclePrev<CR>
nnoremap <leader>j :BuffergatorMruCycleNext<CR>

" Mappings

:set timeout timeoutlen=1000 ttimeoutlen=100

nmap <C-LEFT> <C-W><LEFT>
nmap <C-RIGHT> <C-W><RIGHT>

" F6 - предыдущий буфер
nmap <F6> :bp<cr>
vmap <F6> <esc>:bp<cr>i
imap <F6> <esc>:bp<cr>i

" F7 - следующий буфер
nmap <F7> :bn<cr>
vmap <F7> <esc>:bn<cr>i
imap <F7> <esc>:bn<cr>i

imap <F2> <ESC>:NERDTreeToggle<CR>
map <F2> :NERDTreeToggle<CR>

imap <F4> <ESC>:Bclose<CR> 
map <F4> :Bclose<CR>


" Vim-xkbswitch

" TODO installing automatisation
let g:XkbSwitchEnabled = 1
let g:XkbSwitchIMappings = ['ru']
" fix conflict with DelimitMate
let g:XkbSwitchSkipIMappings = {'*': ["'", '"', '[', ']']}


" Syntastic

let g:syntastic_mode_map = {
\ "mode": "active",
\ "active_filetypes": ["c", "cpp", "java", "javascript", "python", "ruby"],
\ "passive_filetypes": [] }


let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_javascript_eslint_args = "--no-eslintrc --config /mnt/projects/online4/.eslintrc"

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0


let g:ycm_auto_trigger = 0

" Languages related stuff

" Include path for C programs

set path+=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk/usr/include/

" Code folding for JavaScript

let javaScript_fold=1
autocmd BufEnter *.js :normal zR\<Enter\>   "fix initial fold closing


" Integrations

" Proper mouse support inside screen and tmux

set mouse+=a
if &term =~ '^screen'
    " tmux knows the extended mouse mode
    set ttymouse=xterm2
endif

