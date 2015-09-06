set t_Co=256 " color
let g:hybrid_use_Xresources = 1

let mapleader=" "
set showcmd

autocmd VimEnter * color hybrid-material
set background=light

source ~/dotfiles/nvim/filetype-settings.vim
source ~/dotfiles/nvim/vim-plugins.vim
source ~/dotfiles/nvim/plugin-config.vim

set shell=zsh\ -l

set backupdir=~/.vim-tmp
set directory=~/.vim-tmp

set encoding=utf-8

set nowrap
set number

set backspace=2

set clipboard=unnamed

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

set ignorecase
set smartcase
set hlsearch
set incsearch

set history=1000
set undolevels=1000

syntax on
filetype plugin indent on

"force markdown syntax
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

inoremap jj <ESC>

"quick goyo
nnoremap <leader>g :Goyo<CR>

"quick splits
nnoremap <leader>v <c-w>v<c-w>l
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

