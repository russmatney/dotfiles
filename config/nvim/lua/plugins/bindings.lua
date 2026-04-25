local function next_buffer() require("astrocore.buffer").nav(vim.v.count1) end
local function previous_buffer() require("astrocore.buffer").nav(-vim.v.count1) end

local function close_tab()
  require("astroui.status.heirline").buffer_picker(function(bufnr) require("astrocore.buffer").close(bufnr) end)
end

local function switch_session() require("resession").load() end

local function magit() require("neogit").open() end

local function find_commands() require("snacks.picker").commands() end
local function find_commands_history() require("snacks.picker").command_history() end
local function find_autocmds() require("snacks.picker").autocmds() end
local function find_keymaps() require("snacks.picker").keymaps() end
local function search_files() require("snacks.picker").grep() end
local function find_files() require("snacks.picker").files() end

---@type LazySpec
return {
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      mappings = {
        -- normal mode
        n = {
          -- quick save
          ["<Leader><cr>"] = { ":w!<cr>", desc = "russ/save" },
          ["<Leader>R"] = { "<cmd>AstroReload<cr>", desc = "Reload Astrovim config" },

          -- snacks commands
          ["<M-x>"] = { find_commands, desc = "M-x commands" },
          ["<M-r>"] = { find_commands_history, desc = "M-x history" },
          ["<M-a>"] = { find_autocmds, desc = "M-x autocommands" },
          ["<M-X>"] = { find_keymaps, desc = "M-x keymaps" },

          -- project search
          ["<Leader>a"] = { search_files, desc = "russ/search" },
          -- project find-file
          -- TODO how to toggle/include hidden?
          ["<Leader>p"] = { find_files, desc = "russ/open-file" },

          -- ["<Leader>p"] = { ":CtrlP<CR>", desc = "Ctrl-p files" },
          ["<Leader>P"] = { ":CtrlPClearAllCaches<CR>", desc = "Ctrl-p cache clear" },
          ["<Leader>bb"] = { ":CtrlPBuffer<CR>", desc = "Ctrl-p buffers" },
          ["<Leader>bm"] = { ":CtrlPMixed<CR>", desc = "Ctrl-p mixed" },
          ["<Leader>bf"] = { ":CtrlPMRUFiles<CR>", desc = "Ctrl-p MRU files" },

          -- load/switch session/project
          ["<Leader>w"] = { switch_session, desc = "russ/load-session" },

          -- quick terminal
          ["<M-t>"] = { "<cmd>ToggleTerm direction=float<cr>", desc = "Toggle terminal" },

          -- Buffer nav
          ["<Leader><space>"] = { "<C-^>", desc = "last-visited buffer" },
          ["]b"] = { next_buffer, desc = "Next tab" },
          ["[b"] = { previous_buffer, desc = "Previous tab" },
          ["<Leader>l"] = { next_buffer, desc = "Next tab" },
          ["<Leader>h"] = { previous_buffer, desc = "Previous tab" },

          ["<Leader>bd"] = { close_tab, desc = "Close buffer from tabline" },

          -- Kill menu
          ["<Leader>k"] = { desc = "Kill" },
          ["<Leader>kb"] = { desc = "Kill This Buffer" },
          ["<Leader>kf"] = { desc = "Delete This File" },

          -- git
          ["gm"] = { magit, desc = "Magit" },

          -- Plugin Manager
          -- ["<Leader>p"] = false,
          ["<Leader>pi"] = false,
          ["<Leader>ps"] = false,
          ["<Leader>pS"] = false,
          ["<Leader>pu"] = false,
          ["<Leader>pU"] = false,
          ["<Leader>pa"] = false,
          ["<Leader>pm"] = false,
          ["<Leader>pM"] = false,

          -- nuke these bindings so leader-l works without delay
          -- ["<Leader>l"] = false,
          ["<Leader>la"] = false,
          ["<Leader>lA"] = false,

          ["<Leader>ll"] = false,
          ["<Leader>lL"] = false,
          ["<Leader>lf"] = false,

          ["<Leader>lR"] = false,
          ["<Leader>lr"] = false,
          ["<Leader>lh"] = false,

          ["<Leader>lg"] = false,
          ["<Leader>lG"] = false,
          ["<Leader>li"] = false,
          ["<Leader>lI"] = false,
          ["<Leader>ld"] = false,
          ["<Leader>lD"] = false,
          ["<Leader>ls"] = false,
          ["<Leader>lS"] = false,
        },
      },
    },
  },
  {
    "AstroNvim/astrolsp",
    ---@type AstroLSPOpts
    opts = {
      -- mappings to be set up on attaching of a language server
      mappings = {
        n = {
          -- a `cond` key can provided as the string of a server capability to be required to attach, or a function with `client` and `bufnr` parameters from the `on_attach` that returns a boolean
          gD = {
            function() vim.lsp.buf.declaration() end,
            desc = "Declaration of current symbol",
            cond = "textDocument/declaration",
          },
          ["<Leader>uY"] = {
            function() require("astrolsp.toggles").buffer_semantic_tokens() end,
            desc = "Toggle LSP semantic highlight (buffer)",
            cond = function(client)
              return client.supports_method "textDocument/semanticTokens/full" and vim.lsp.semantic_tokens ~= nil
            end,
          },

          -- nuke these bindings so leader-l works without delay
          -- ["<Leader>l"] = false,
          ["<Leader>la"] = false,
          ["<Leader>lA"] = false,

          ["<Leader>ll"] = false,
          ["<Leader>lL"] = false,
          ["<Leader>lf"] = false,

          ["<Leader>lR"] = false,
          ["<Leader>lr"] = false,
          ["<Leader>lh"] = false,

          ["<Leader>lg"] = false,
          ["<Leader>lG"] = false,
          ["<Leader>li"] = false,
          ["<Leader>lI"] = false,
          ["<Leader>ld"] = false,
          ["<Leader>lD"] = false,
          ["<Leader>ls"] = false,
          ["<Leader>lS"] = false,
        },
      },
    },
  },
}
