local wk = require("which-key")

-- to reload, should just be:
-- :luafile lua/bindings.lua

-- Leader prefix
wk.register({
  o = {"<cmd>hi Normal guibg=None ctermbg=None<CR>", "transparent bg"},

  -- quick file open
  e = {
    i = {":e ~/.config/nvim/init.vim<CR>", "init.vim"},
    c = {":e ~/.config/nvim/config.vim<CR>", "config.vim"},
    b = {":e ~/.config/nvim/lua/bindings.lua<CR>", "bindings.lua"},
    p = {":e ~/.config/nvim/lua/plugins.lua<CR>", "plugins.lua"}
  },

  p = {"<cmd>Telescope find_files<cr>", "Find File"},
  f = {
    b = {"<cmd>Telescope file_browser<cr>", "Browse Files"},
    f = {"<cmd>Telescope find_files<cr>", "Find File"},
    g = {"<cmd>Telescope git_status<cr>", "Git files"},
    a = {"<cmd>Telescope live_grep<cr>", "Search Files"},
    b = {"<cmd>Telescope buffers<cr>", "Find Buffer"},
    h = {"<cmd>Telescope help_tags<cr>", "Telescope Help"}
  },
  x = {"<cmd>Telescope commands<cr>", "Commands (M-x)"},
  t = {"<cmd>Telescope<cr>", "Telescope root"}

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
