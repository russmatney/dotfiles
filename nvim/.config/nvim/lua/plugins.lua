vim.cmd [[packadd packer.nvim]]

-- To reload: luafile lua/plugins.lua
-- then: :PackerSync
-- TODO write command and binding for this

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'

  -- Lua
  use 'andrejlevkovitch/vim-lua-format'
  use_rocks {'luaformatter', server = 'https://luarocks.org/dev'}
  use_rocks {'penlight', 'lua-cjson', 'lua-resty-http', 'lpeg'}

  -- Misc Repl support
  use 'Olical/conjure'

  -- Fennel
  use 'Olical/aniseed'

  -- Clojure
  use 'clojure-vim/vim-jack-in'

  -- Local
  -- use '~/russmatney/some-nvim-plugin/blah.nvim'

  -- NeoVim infra
  use 'famiu/nvim-reload'
  use 'tpope/vim-dispatch'
  use 'radenling/vim-dispatch-neovim'

  -- Misc Syntax Plugins
  use 'vim-scripts/SyntaxComplete'
  use 'cespare/vim-toml'

  -- Git
  use 'tpope/vim-fugitive'

  -- Visual Aid
  use 'nathanaelkane/vim-indent-guides'
  use 'rschmukler/pangloss-vim-indent'
  use 'junegunn/goyo.vim'
  use 'junegunn/limelight.vim'
  use 'vim-airline/vim-airline'
  use 'vim-airline/vim-airline-themes'

  -- Keybindings
  use {
    "folke/which-key.nvim",
    config = function() require("which-key").setup {} end
  }

  -- Finders
  use 'kien/ctrlp.vim'
  use {'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/plenary.nvim'}}}
  use 'scrooloose/nerdtree'
  use 'ton/vim-bufsurf'
  use 'yegappan/mru'
  use 'tweekmonster/fzf-filemru'
  use '~/.fzf'
  use 'junegunn/fzf.vim'

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

  -- Search
  use 'rking/ag.vim'

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
