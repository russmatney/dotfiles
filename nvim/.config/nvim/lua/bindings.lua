local wk = require("which-key")

-- Leader prefix
wk.register({
  o = {"<cmd>hi Normal guibg=None ctermbg=None<CR>", "transparent bg"},

  -- quick file open
  e = {
    i = {":e ~/.config/nvim/init.vim<CR>", "init.vim"},
    c = {":e ~/.config/nvim/config.vim<CR>", "config.vim"},
    b = {":e ~/.config/nvim/lua/bindings.lua<CR>", "bindings.lua"},
    p = {":e ~/.config/nvim/lua/plugins.lua<CR>", "plugins.lua"}
  }

}, {prefix = "<leader>"})

-- navigation
wk.register({
  ["<A-h>"] = {"<cmd>TmuxNavigateLeft<cr>", "Nav left"},
  ["<A-l>"] = {"<cmd>TmuxNavigateRight<cr>", "Nav right"},
  ["<A-k>"] = {"<cmd>TmuxNavigateUp<cr>", "Nav up"},
  ["<A-j>"] = {"<cmd>TmuxNavigateDown<cr>", "Nav down"},
  ["<A-\\>"] = {"<cmd>TmuxNavigatePrevious<cr>", "Nav prev"},
  ["<A-|>"] = {"<cmd>TmuxNavigateNext<cr>", "Nav next"}
})
