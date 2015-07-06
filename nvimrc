set t_Co=256 " color

let mapleader=" "
set showcmd

source ~/dotfiles/nvim/filetype-settings.vim
source ~/dotfiles/nvim/vim-plugins.vim

set shell=zsh\ -l

set backupdir=~/.vim-tmp
set directory=~/.vim-tmp

set encoding=utf-8

set nowrap
set number

set backspace=2

" indents and tab stuff
set smartindent
set tabstop=2
set shiftwidth=2
set softtabstop=2
set shiftround
set smarttab
set autoindent
set copyindent
set expandtab

set clipboard=unnamed

set ignorecase
set smartcase
set hlsearch
set incsearch

set history=1000
set undolevels=1000

filetype plugin indent on
syntax on

inoremap jj <ESC>

"quick splits
nnoremap <leader>vs <c-w>v<c-w>l
nnoremap <leader>s <c-w>s<c-w>k

"quick swap
nnoremap <leader><leader> <c-^>

"align
nnoremap <leader>a =ip

"open file in same dir
nnoremap <leader>E :Explore<CR>

"window resizing
nnoremap <leader>l <c-w>10<
nnoremap <leader>h <c-w>10>
