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
  (use "wbthomason/packer.nvim")

  (use "kyazdani42/nvim-web-devicons")
  (use {
        1 "goolord/alpha-nvim"
    :config (fn []
         ;; TODO not really working yet....
         (local alpha (require "alpha"))
         (local dashboard (require "alpha.themes.dashboard"))

         (set dashboard.section.header.val ["NEOVIM" "neovim" "nEoViM" "NeOvIm"])
         (set dashboard.section.buttons.val [
             (dashboard.button "e" "  New file" ":ene <BAR> startinsert <CR>")
             (dashboard.button "q" "  Quit NVIM" ":qa<CR>")
             (dashboard.button :p "PackerSync" "<cmd>PackerSync<CR>")
             (dashboard.button :c "PackerCompile" "<cmd>PackerCompile<CR>")
             (dashboard.button :i "Impatient Cache Clear" ":LuaCacheClear<CR>")
             (dashboard.button :m "Messages" ":messages<CR>")
         ])
         (set dashboard.opts.opts.noautocmd true)
         (vim.cmd "autocmd User AlphaReady echo 'ready'")
         (alpha.setup dashboard.opts))})

  ; Lua
  (use "andrejlevkovitch/vim-lua-format")

  (use_rocks {1 "luaformatter" :server "https://luarocks.org/dev"})
  (use_rocks ["penlight" "lua-cjson" "lua-resty-http" "lpeg"])

  ; Repl support
  (use "Olical/conjure")
  (use "Olical/aniseed")

  ; Clojure
  (use "clojure-vim/vim-jack-in")

  ; NeoVim infra
  (use "famiu/nvim-reload")
  (use "tpope/vim-dispatch")
  (use "radenling/vim-dispatch-neovim")

  ; Misc Syntax Plugins
  (use "vim-scripts/SyntaxComplete")
  (use "cespare/vim-toml")

  ; Git
  (use "tpope/vim-fugitive")

  ; Visual Aid
  (use "nathanaelkane/vim-indent-guides")
  (use "rschmukler/pangloss-vim-indent")
  (use "junegunn/goyo.vim")
  (use "junegunn/limelight.vim")
  (use "vim-airline/vim-airline")
  (use "vim-airline/vim-airline-themes")

  ; Keybindings
  (use {
    1 "folke/which-key.nvim"
    :config (fn [] (let [wk (require "which-key")]
                     (wk:setup {}))) })

  ; Finders
  (use "kien/ctrlp.vim")
  (use "nvim-lua/plenary.nvim")
  (use "nvim-telescope/telescope.nvim")
  (use "scrooloose/nerdtree")
  (use "ton/vim-bufsurf")
  (use "yegappan/mru")
  (use "tweekmonster/fzf-filemru")
  (use "~/.fzf")
  (use "junegunn/fzf.vim")

  ; Alignment
  (use "godlygeek/tabular")

  ; Util
  (use "tpope/vim-surround")
  (use "tpope/vim-repeat")
  (use "tpope/vim-abolish")
  (use "tpope/vim-commentary")
  (use "sjl/clam.vim")
  (use "terryma/vim-multiple-cursors")

  ; Navigation
  (use "christoomey/vim-tmux-navigator")
  (use "vimoutliner/vimoutliner")
  (use "vim-scripts/BufOnly.vim")

  ; Color
  (use "Slava/vim-colors-tomorrow")
  (use "jscappini/material.vim")
  (use "mhartington/oceanic-next")
  (use "roosta/srcery")
  (use "flazz/vim-colorschemes")

  ; Search
  (use "rking/ag.vim")

  (use "lewis6991/impatient.nvim")
  (use {
    1 "lewis6991/gitsigns.nvim"
    :config (fn []
              (let [gitsigns (require "gitsigns")]
                (gitsigns:setup {})))})
)
; :config {:compile_path (.. (vim.fn.stdpath "config") "/lua/packer_compiled.lua") }
}
))


;; Run script/sync.sh to update, install and clean your plugins.
;; Packer configuration format: https://github.com/wbthomason/packer.nvim
;; (plugin.use
;;   :Olical/aniseed {}
;;   :Olical/conjure {}
;;   :PeterRincker/vim-argumentative {}
;;   :airblade/vim-gitgutter {}
;;   :clojure-vim/clojure.vim {}
;;   :clojure-vim/vim-jack-in {}
;;   :easymotion/vim-easymotion {}
;;   :folke/which-key.nvim {}
;;   :guns/vim-sexp {}
;;   :hrsh7th/nvim-compe {}
;;   :itchyny/lightline.vim {}
;;   :jiangmiao/auto-pairs {:mod :auto-pairs}
;;   :junegunn/fzf {}
;;   :junegunn/fzf.vim {}
;;   :liuchengxu/vim-better-default {:mod :better-default}
;;   :mbbill/undotree {}
;;   :radenling/vim-dispatch-neovim {}
;;   :srcery-colors/srcery-vim {:mod :srcery}
;;   :tami5/compe-conjure {}
;;   :tpope/vim-abolish {}
;;   :tpope/vim-commentary {}
;;   :tpope/vim-dispatch {}
;;   :tpope/vim-eunuch {}
;;   :tpope/vim-fugitive {}
;;   :tpope/vim-repeat {}
;;   :tpope/vim-sexp-mappings-for-regular-people {}
;;   :tpope/vim-sleuth {}
;;   :tpope/vim-surround {}
;;   :tpope/vim-unimpaired {}
;;   :tpope/vim-vinegar {}
;;   :w0rp/ale {:mod :ale}
;;   :wbthomason/packer.nvim {}

;;   :andrejlevkovitch/vim-lua-format {}
;;   :nvim-lua/plenary.nvim {}

;;   :neovim/nvim-lspconfig {}
;;   :hrsh7th/nvim-cmp {}
;;   :hrsh7th/cmp-nvim-lsp {}
;;   :saadparwaiz1/cmp_luasnip {}
;;   :L3MON4D3/LuaSnip {}

;;   :TimUntersberger/neogit {}

;;   :kien/ctrlp.vim {}
;;   :scrooloose/nerdtree {}
;;   :ton/vim-bufsurf {}
;;   :yegappan/mru {}
;;   :tweekmonster/fzf-filemru {}
;;   :rking/ag.vim {}
;;   :godlygeek/tabular {}

;;   :machakann/vim-sandwich {}
;;   :sjl/clam.vim {}
;;   :terryma/vim-multiple-cursors {}
;;   :tpope/vim-endwise {}
;;   :nvim-treesitter/nvim-treesitter {:run ":TSUpdate"}

;;   :christoomey/vim-tmux-navigator {}
;;   :vimoutliner/vimoutliner {}
;;   :vim-scripts/BufOnly.vim {}

;;   :tami5/sqlite.lua {}
;;   :nvim-telescope/telescope-frecency.nvim {}
;;   :nvim-telescope/telescope-fzf-native.nvim {:run "make"}
;;   :nvim-telescope/telescope.nvim {}
;;   :nvim-telescope/telescope-packer.nvim {}
;;   :sudormrfbin/cheatsheet.nvim {}
;;   ; :rmagatti/session-lens {}
;;   ; :rmagatti/auto-session {}
;;   :GustavoKatel/telescope-asynctasks.nvim {}
;;   :skywind3000/asynctasks.vim {}
;;   :skywind3000/asyncrun.vim {}
;;   )


{: setup_packages}
