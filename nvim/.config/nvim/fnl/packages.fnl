(module packages)

(local a (require "aniseed.core"))
(local packer (require "packer"))

(a.println "evaling packages.fnl")

(vim.cmd "packadd packer.nvim")

(comment
  (lua nil "{'some-str', key = 'val'}")
  (lua nil "'hi'")

  ;; not equal b/c tables are unique, and = checks identity
  (= {1 :some-str :key "val"} (lua nil "{'some-str', key = 'val'}")))

(fn setup_packages []
  (a.println "setup_packages")

  (packer.startup
    { 1 (fn [use]

          (use_rocks {1 :luaformatter :server "https://luarocks.org/dev"})
          (use_rocks ["penlight" "lua-cjson" "lua-resty-http" "lpeg"])

          (use :airblade/vim-gitgutter)
          (use :andrejlevkovitch/vim-lua-format)
          (use :christoomey/vim-tmux-navigator)
          (use :clojure-vim/clojure.vim)
          (use :clojure-vim/vim-jack-in)
          (use :flazz/vim-colorschemes)
          (use {
                1 :folke/which-key.nvim
                :config (fn [] (let [wk (require "which-key")]
                                 (wk:setup {}))) })
          (use {
                1 :goolord/alpha-nvim
                :config (fn []
                          ;; TODO not really that great yet....
                          (local alpha (require "alpha"))
                          (local dashboard (require "alpha.themes.dashboard"))

                          (set dashboard.section.header.val ["NEOVIM" "neovim" "nEoViM" "NeOvIm"])
                          (set dashboard.section.buttons.val 
                               [(dashboard.button "e" "  New file" ":ene <BAR> startinsert <CR>")
                                (dashboard.button "q" "  Quit NVIM" ":qa<CR>")
                                (dashboard.button :p "PackerSync" "<cmd>PackerSync<CR>")
                                (dashboard.button :c "PackerCompile" "<cmd>PackerCompile<CR>")
                                (dashboard.button :i "Impatient Cache Clear" ":LuaCacheClear<CR>")
                                (dashboard.button :m "Messages" ":messages<CR>")
                                ])
                          (set dashboard.opts.opts.noautocmd true)
                          (vim.cmd "autocmd User AlphaReady echo 'ready'")
                          (alpha.setup dashboard.opts))})
          (use {1 :guns/vim-sexp
                :config (fn [] (set vim.g.sexp_filetypes "clojure,fennel")) })
          (use :GustavoKatel/telescope-asynctasks.nvim)
          (use :hrsh7th/nvim-compe)
          (use :itchyny/lightline.vim)
          (use :jiangmiao/auto-pairs)
          (use :jscappini/material.vim)
          (use :junegunn/fzf)
          (use :junegunn/fzf.vim)
          (use :kyazdani42/nvim-web-devicons)
          (use :lewis6991/impatient.nvim)
          (use :mhartington/oceanic-next)
          (use :nathanaelkane/vim-indent-guides)
          (use :nvim-lua/plenary.nvim)
          (use :nvim-telescope/telescope-frecency.nvim)
          (use :nvim-telescope/telescope-fzf-native.nvim)
          (use :nvim-telescope/telescope-packer.nvim)
          (use {1 :nvim-telescope/telescope.nvim
                :config (fn []
                          (local ts (require "telescope"))
                          (ts.setup
                            {:defaults {}
                             :pickers {}
                             :extensions {}})) })
          (use :Olical/aniseed)
          (use :Olical/conjure)
          (use :radenling/vim-dispatch-neovim)
          (use :rking/ag.vim)
          (use :Slava/vim-colors-tomorrow)
          (use :skywind3000/asyncrun.vim)
          (use :skywind3000/asynctasks.vim)
          (use :sudormrfbin/cheatsheet.nvim)
          (use :tami5/compe-conjure)
          (use :tami5/sqlite.lua)
          (use :tpope/vim-abolish)
          (use :tpope/vim-commentary)
          (use :tpope/vim-dispatch)
          (use :tpope/vim-eunuch)
          (use :tpope/vim-fugitive)
          (use :tpope/vim-repeat)
          (use :tpope/vim-sexp-mappings-for-regular-people)
          (use :tpope/vim-sleuth)
          (use :tpope/vim-surround)
          (use :tpope/vim-unimpaired)
          (use :tpope/vim-vinegar)
          (use :vim-airline/vim-airline)
          (use :vim-airline/vim-airline-themes)
          (use :vim-scripts/BufOnly.vim)
          (use :vim-scripts/SyntaxComplete)
          (use :vimoutliner/vimoutliner)
(use :w0rp/ale)
(use :wbthomason/packer.nvim)

)
; :config {:compile_path (.. (vim.fn.stdpath "config") "/lua/packer_compiled.lua") }
}
))

{: setup_packages}
