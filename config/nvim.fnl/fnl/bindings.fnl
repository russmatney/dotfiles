(module bindings)

(fn setup-keybindings []
  ;; whick-key supported
  (local wk (require "which-key"))

  (wk.register
    {:o ["<cmd>hi Normal guibg=None ctermbg=None<CR>" "Transparent BG"]}
    {:prefix "<leader>"})

  (wk.register
    {
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
     "," ["<cmd>Telescope find_files<cr>" "Find Files"]
     } {:prefix "<leader>"})

  (wk.register
    {
     "<A-;>" ["<cmd>Telescope command_history<cr>" "Command History"]
     :<A-x> [;; TODO impl proper M-x that sources command_history, commands, misc functions
             "<cmd>Telescope commands<cr>" "M-x (Commands)"]
     :<A-f> ["<cmd>Telescope file_browser<cr>" "Browse Files"]
     "<A-,>" ["<cmd>Telescope find_files<cr>" "Find Files"]
     :<A-b> ["<cmd>Telescope buffers<cr>" "Find Buffer"]
     :<A-t> ["<cmd>Telescope<cr>" "Telescope root"]})

  ; (wk.register {
  ;   :<A-h> ["<cmd>TmuxNavigateLeft<cr>" "Nav left"]
  ;   :<A-l> ["<cmd>TmuxNavigateRight<cr>" "Nav right"]
  ;   :<A-k> ["<cmd>TmuxNavigateUp<cr>" "Nav up"]
  ;   :<A-j> ["<cmd>TmuxNavigateDown<cr>" "Nav down"]
  ;   :<A-\> ["<cmd>TmuxNavigatePrevious<cr>" "Nav prev"]
  ;   :<A-|> ["<cmd>TmuxNavigateNext<cr>" "Nav next"]
  ; })
  )

{: setup-keybindings}
