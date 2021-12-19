(module core
  {autoload
   {a aniseed.core
    nvim aniseed.nvim}})

(a.println "evaling core.fnl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; bindings

(fn keymap [...]
  (vim.api.nvim_set_keymap ...))

(set nvim.g.mapleader " ")
(set nvim.g.maplocalleader ",")

;; non-negotiables

; reload everything (source this file)
(keymap "n" "gr" ":so ~/.config/nvim/init.lua<CR>" {:noremap true})

; file save
(keymap "n" "<leader><CR>" ":w<CR>" {:noremap true})

; toggle file
(keymap "n" "<leader><leader>" "<c-^>" {:noremap true})

; quick dir nav
(keymap "n" "-" ":Explore<CR>" {:noremap true})

; unhighlight
(keymap "n" "<CR>" ":noh<CR><CR>" {:noremap true})

; splits
(keymap "n" "<leader>v" "<c-w>v<c-w>" {:noremap true})
(keymap "n" "<leader>s" "<c-w>s<c-w>k" {:noremap true})

(fn setup_keybindings []
  (a.println "setup_keybindings")
;; whick-key supported

(local wk (require "which-key"))

(wk.register
  {:o ["<cmd>hi Normal guibg=None ctermbg=None<CR>" "Transparent BG"]}
  {:prefix "<leader>"})

(wk.register {
  :e {
    :i [":e ~/.config/nvim/init.lua<cr>" "init.lua"]
    :c [":e ~/.config/nvim/fnl/core.fnl<cr>" "core.fnl"]
    :b [":e ~/.config/nvim/fnl/bindings.fnl<cr>" "bindings.fnl"]
    :p [":e ~/.config/nvim/fnl/packages.fnl<cr>" "packages.fnl"]
  }
  :p ["<cmd>Telescope find_files<cr>" "Open File"]
  :a ["<cmd>Telescope live_grep<cr>" "Search Files"]
  :h ["<cmd>Telescope help_tags<cr>" "Telescope Help"]
  :d ["<cmd>Alpha<cr>" "Dashboard"]

} {:prefix "<leader>"})

(wk.register {
  "<A-;>" ["<cmd>Telescope command_history<cr>" "Command History"]
  :<A-x> [;; TODO impl proper M-x that sources command_history, commands, misc functions
          "<cmd>Telescope commands<cr>" "M-x (Commands)"]
  :<A-f> ["<cmd>Telescope file_browser<cr>" "Browse Files"]
  "<A-,>" ["<cmd>Telescope find_files<cr>" "Command History"]
  :<A-b> ["<cmd>Telescope buffers<cr>" "Find Buffer"]
  :<A-t> ["<cmd>Telescope<cr>" "Telescope root"]
})

(wk.register {
  :<A-h> ["<cmd>TmuxNavigateLeft<cr>" "Nav left"]
  :<A-l> ["<cmd>TmuxNavigateRight<cr>" "Nav right"]
  :<A-k> ["<cmd>TmuxNavigateUp<cr>" "Nav up"]
  :<A-j> ["<cmd>TmuxNavigateDown<cr>" "Nav down"]
  :<A-\> ["<cmd>TmuxNavigatePrevious<cr>" "Nav prev"]
  :<A-|> ["<cmd>TmuxNavigateNext<cr>" "Nav next"]
}))

{: setup_keybindings}
