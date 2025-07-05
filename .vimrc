" Set the leader key to comma
let mapleader = " "

inoremap jk <Esc>
inoremap jj <Esc>

" Enable syntax highlighting
syntax on

" Set Vim to use spaces instead of tabs
set expandtab
        
" Set the number of spaces tabs represent (default is 8)
set tabstop=4
set shiftwidth=4
set softtabstop=4

" Enable line numbering
set number

" Enable relative numbering
set relativenumber

" Highlight the current line
set cursorline

" Enable mouse support in all modes
set mouse=a

" Turn on the file type detection. Vim will try to guess the file type.
filetype plugin on
filetype indent on


" Turn on the file type detection. Vim will try to guess the file type.
filetype plugin on
filetype indent on

" Set the search pattern to be case insensitive unless uppercase is used
set ignorecase
set smartcase

" Enable the clipboard to be shared between Vim and the system
set clipboard=unnamed

" Turn on spell checking for English
set spell spelllang=en_us

" Enable folding based on indentation
set foldmethod=indent
set foldlevelstart=10

" Map <leader>w to save
nnoremap <leader>w :w<CR>


set hlsearch

nnoremap <leader>vi :e ~/.vimrc<CR>

call plug#begin()
Plug 'junegunn/seoul256.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
" Use 'dir' option to install plugin in a non-default directory
call plug#end()
" Light color scheme
" Unified color scheme (default: dark)
colo seoul256

set laststatus=2
set statusline=%f\ %P
