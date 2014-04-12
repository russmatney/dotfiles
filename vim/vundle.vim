set nocompatible
filetype on
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

" Themes
Bundle 'cseelus/vim-colors-clearance'
Bundle 'w0ng/vim-hybrid'

" Syntax Plugins
Bundle 'pangloss/vim-javascript'
Bundle 'othree/html5.vim'
Bundle 'uggedal/go-vim'
Bundle 'digitaltoad/vim-jade'
Bundle 'kchmck/vim-coffee-script'

" Utilities
Bundle 'scrooloose/nerdtree'
Bundle 'The-NERD-Commenter'
Bundle 'airblade/vim-gitgutter'
Bundle 'mattn/emmet-vim'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-repeat'
Bundle 'kien/ctrlp.vim'
Bundle 'nathanaelkane/vim-indent-guides'
Bundle 'christoomey/vim-tmux-navigator'

