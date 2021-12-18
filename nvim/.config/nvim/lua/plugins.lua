vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'

  -- Lua
  use 'andrejlevkovitch/vim-lua-format'
  use_rocks {'luaformatter', server = 'https://luarocks.org/dev'}
  use_rocks {'penlight', 'lua-cjson', 'lua-resty-http', 'lpeg'}

  -- Local
  -- use '~/russmatney/some-nvim-plugin/blah.nvim'

  -- Vim lifecycle
  use 'famiu/nvim-reload'

  -- Misc Syntax Plugins
  use 'vim-scripts/SyntaxComplete'
  use 'rust-lang/rust.vim'
  use 'racer-rust/vim-racer'
  use 'cespare/vim-toml'

  -- Git
  use 'tpope/vim-fugitive'

  -- Visual Aid
  use 'nathanaelkane/vim-indent-guides'
  use 'rschmukler/pangloss-vim-indent'
  use 'junegunn/goyo.vim'
  use 'junegunn/limelight.vim'
  use 'othree/yajs.vim'
  use 'othree/javascript-libraries-syntax.vim'
  use 'othree/es.next.syntax.vim'
  use 'Quramy/vim-js-pretty-template'
  use {
    "folke/which-key.nvim",
    config = function() require("which-key").setup {} end
  }

  -- Alignment
  use 'godlygeek/tabular'

  -- Util
  use 'tpope/vim-surround'
  use 'tpope/vim-repeat'
  use 'tpope/vim-abolish'
  use 'tpope/vim-commentary'
  use 'sjl/clam.vim'
  use 'terryma/vim-multiple-cursors'
  use 'tpope/vim-endwise'

  -- Navigation
  use 'christoomey/vim-tmux-navigator'
  use 'kien/ctrlp.vim'
  use 'vim-airline/vim-airline'
  use 'vim-airline/vim-airline-themes'
  use 'scrooloose/nerdtree'
  use 'ton/vim-bufsurf'
  use 'yegappan/mru'
  use 'tweekmonster/fzf-filemru'
  use '~/.fzf'
  use 'junegunn/fzf.vim'

  use 'vimoutliner/vimoutliner'

  use 'vim-scripts/BufOnly.vim'

  -- Color
  use 'morhetz/gruvbox'
  use 'Slava/vim-colors-tomorrow'
  use 'jscappini/material.vim'
  use 'mhartington/oceanic-next'
  use 'NLKNguyen/papercolor-theme'
  use 'cseelus/vim-colors-lucid'
  use 'bcicen/vim-vice'
  use 'roosta/srcery'
  use 'flazz/vim-colorschemes'

  -- Docs
  use 'rizzatti/dash.vim'

  -- Search
  use 'rking/ag.vim'

  -- Proc managers
  use 'benekastah/neomake'
  -- use 'Shougo/vimproc.vim', {'do': 'make'}

  -- Web
  use 'pangloss/vim-javascript'
  use 'othree/html5.vim'
  use 'digitaltoad/vim-jade'
  use 'rschmukler/typescript-vim'
  use 'Quramy/tsuquyomi'
  use 'marijnh/tern_for_vim'

  -- Elixir
  use 'elixir-lang/vim-elixir'
  use 'slashmili/alchemist.vim'
  use 'powerman/vim-plugin-AnsiEsc'

  -- Elm
  use 'elmcast/elm-vim'

  -- Completion
  use 'ervandew/supertab'
  use 'mattn/emmet-vim'
  use 'Valloric/YouCompleteMe'

  use {
    'lewis6991/gitsigns.nvim',
    requires = {'nvim-lua/plenary.nvim'},
    config = function() require('gitsigns').setup() end
  }
end)
