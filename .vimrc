set nocompatible
set t_Co=256

set laststatus=2
python from powerline.vim import setup as powerline_setup
python powerline_setup()
python del powerline_setup

let g:Powerline_symbols = 'fancy'
set autochdir
set tags+=./tags;
set backupdir=/tmp//
set directory=/tmp//


