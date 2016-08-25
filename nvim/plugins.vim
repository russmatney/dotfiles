if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/dotfiles/nvim/plugins')

" Syntax Plugins
Plug 'pangloss/vim-javascript'
Plug 'othree/html5.vim'
Plug 'digitaltoad/vim-jade'
Plug 'vim-scripts/SyntaxComplete'
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'
Plug 'cespare/vim-toml'
Plug 'lambdatoast/elm.vim', { 'for': 'elm' }

" Git
Plug 'tpope/vim-fugitive'

" Shortcuts
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-abolish'

"Visual Aid
Plug 'nathanaelkane/vim-indent-guides'
Plug 'rschmukler/pangloss-vim-indent'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'othree/yajs.vim'
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'othree/es.next.syntax.vim'
Plug 'Quramy/vim-js-pretty-template'

Plug 'tpope/vim-commentary'

" Navigation
Plug 'christoomey/vim-tmux-navigator'
Plug 'kien/ctrlp.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'scrooloose/nerdtree'

Plug 'vimoutliner/vimoutliner'

" Color
Plug 'morhetz/gruvbox'
Plug 'Slava/vim-colors-tomorrow'
Plug 'jscappini/material.vim'
Plug 'mhartington/oceanic-next'
Plug 'NLKNguyen/papercolor-theme'

" Search
Plug 'rking/ag.vim'

" Proc managers
Plug 'benekastah/neomake'
Plug 'Shougo/vimproc.vim'

" TypeScript
Plug 'rschmukler/typescript-vim'
Plug 'Quramy/tsuquyomi'
Plug 'marijnh/tern_for_vim'

" Golang
Plug 'fatih/vim-go', { 'for': 'go' }
Plug 'rhysd/vim-go-impl', { 'for': 'go' }
Plug 'majutsushi/tagbar', { 'for': 'go' }
Plug 'nsf/gocode', { 'rtp': 'nvim', 'do': '~/dotfiles/nvim/plugins/gocode/nvim/symlink.sh' }
function! DoRemote(arg)
  UpdateRemotePlugins
endfunction
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') }
Plug 'zchee/deoplete-go', { 'do': 'make' }

" Elixir
Plug 'elixir-lang/vim-elixir'
Plug 'slashmili/alchemist.vim'
Plug 'powerman/vim-plugin-AnsiEsc'

" Completion
Plug 'ervandew/supertab'
Plug 'mattn/emmet-vim'

call plug#end()
