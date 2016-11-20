set t_Co=256
let g:hybrid_use_Xresources = 1
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

autocmd VimEnter * color OceanicNext
" autocmd VimEnter * color srcery
" autocmd VimEnter * color hybrid-material
" autocmd VimEnter * color tomorrow
set background=dark

let mapleader=" "
"set showcmd
set hidden

source ~/dotfiles/nvim/plugins.vim
source ~/dotfiles/nvim/filetype-settings.vim
source ~/dotfiles/nvim/config.vim

set completeopt=longest,menuone,preview

autocmd BufWritePre * %s/\s\+$//e

set shell=zsh

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

set wildmode=longest,list,full

set ignorecase
set smartcase
set hlsearch
set incsearch

set history=1000
set undolevels=1000

syntax on
syntax enable
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
"nnoremap <leader>l <c-w>10<
"nnoremap <leader>h <c-w>10>

"rerun syntax highlighting (helpful for large files)
nmap <leader>f :syntax sync fromstart<CR>

"for those mofoking binaries
"nmap <leader>j :setf javascript<CR>

"unhighlight
nmap <leader>h :nohlsearch<CR>

"move split in direction
nmap <leader>H <C-W>H
nmap <leader>L <C-W>L
nmap <leader>K <C-W>K
nmap <leader>J <C-W>J

" // to search for visually selected text
vnoremap // y/<C-R>"<CR>

nmap <leader>r :redraw!<CR>
nmap <leader>s :syntax sync fromstart<CR>

nnoremap <silent> <Leader>+ :exe "resize " . (winheight(0) * 3/2)<CR>
nnoremap <silent> <Leader>- :exe "resize " . (winheight(0) * 2/3)<CR>


" Custom commands - ripped from rschmukler
command -nargs=1 DE :e `dirname %`/<args>

