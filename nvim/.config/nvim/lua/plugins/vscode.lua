-- don't do anything in non-vscode instances
if not vim.g.vscode then return {} end

-- a list of known working plugins with vscode-neovim, update with your own plugins
local plugins = {
  "lazy.nvim",
  "AstroNvim",
  "astrocore",
  "astroui",
  "Comment.nvim",
  "nvim-autopairs",
  "nvim-treesitter",
  "nvim-ts-autotag",
  "nvim-treesitter-textobjects",
  "nvim-ts-context-commentstring",
}

local Config = require "lazy.core.config"
-- disable plugin update checking
Config.options.checker.enabled = false
Config.options.change_detection.enabled = false
-- replace the default `cond`
Config.options.defaults.cond = function(plugin) return vim.tbl_contains(plugins, plugin.name) end

-- prevent neovim messages opening vscode output (not working!?)
-- https://stackoverflow.com/questions/78611905/turn-off-neovim-messages-in-vscode
-- https://github.com/vscode-neovim/vscode-neovim/issues/2099
vim.g.cmdheight = 0

---@type LazySpec
return {
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      mappings = {
        n = {
          -- bindings i'd love to write here but are otherwise just added to vscode directly
          -- ["<Alt-x>"] = "<CMD>call VSCodeNotify('workbench.action.showCommands')<CR>",

          -- leader key bindings
          -- (vscode doesn't handle chords starting with Space)

          -- leader keys to disable
          ["<Leader>c"] = false, -- closing the window makes vscode very confused

          -- filetree
          ["<Leader>e"] = "<CMD>call VSCodeNotify('workbench.view.explorer')<CR>",
          ["<Leader>o"] = "<CMD>call VSCodeNotify('workbench.files.action.focusFilesExplorer')<CR>",

          -- quick save
          ["<Leader><CR>"] = "<CMD>call VSCodeNotify('workbench.action.files.saveAll')<CR>",

          -- buffer nav
          ["<Leader><Space>"] = "<CMD>call VSCodeNotify('workbench.action.openPreviousEditorFromHistory')<CR>",
          ["[b"] = "<CMD>call VSCodeNotify('workbench.action.previousEditor')<CR>",
          ["]b"] = "<CMD>call VSCodeNotify('workbench.action.nextEditor')<CR>",
          ["<Leader>h"] = "<CMD>call VSCodeNotify('workbench.action.previousEditor')<CR>",
          ["<Leader>l"] = "<CMD>call VSCodeNotify('workbench.action.nextEditor')<CR>",

          -- project find-file
          ["<Leader>ff"] = "<CMD>Find<CR>",
          ["<Leader>p"] = "<CMD>Find<CR>",

          -- project search
          ["<Leader>a"] = "<CMD>call VSCodeNotify('workbench.action.findInFiles')<CR>",

          -- project switch
          ["<Leader>w"] = "<CMD>call VSCodeNotify('projectManager.listProjects')<CR>",

          -- magit
          ["gm"] = "<CMD>call VSCodeNotify('magit.status')<CR>",
        },
      },
    },
  },
  -- disable colorscheme setting
  { "AstroNvim/astroui", opts = { colorscheme = false } },
  -- disable treesitter highlighting
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { highlight = { enable = false } },
  },
}
