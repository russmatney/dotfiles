set t_Co=256 " color

source ~/.vim/filetype_settings.vim
source ~/.vim/plugin_config.vim
source ~/.vim/vundle.vim

set backupdir=~/.vim-tmp
set directory=~/.vim-tmp

set encoding=utf-8

set nowrap
set relativenumber

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

" Paste toggle
set pastetoggle=<leader>p

" Clipboard
set clipboard=unnamed

set showmatch " show matching parens

" search settings - highlighting and case smarts
set ignorecase
set smartcase
set hlsearch
set incsearch

set history=1000
set undolevels=1000

"color solarized
"color Tomorrow-Night
color wombat
set background=light

let g:html_indent_inctags = "html,body,head,tbody"
let g:html_indent_script1 = "inc"
let g:html_indent_style1 = "inc"

filetype plugin indent on
syntax on

" make switch to normal mode easier
inoremap jj <ESC>

" Make sure we hilight extra whitespace in the most annoying way possible.
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()
