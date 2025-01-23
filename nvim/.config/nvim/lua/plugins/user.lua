-- You can also add or configure plugins by creating files in this `plugins/` folder
-- Here are some examples:

---@type LazySpec
return {

  -- == Examples of Adding Plugins ==

  -- "andweeb/presence.nvim",
  {
    "ray-x/lsp_signature.nvim",
    event = "BufRead",
    config = function() require("lsp_signature").setup() end,
  },

  -- == Examples of Overriding Plugins ==

  -- customize alpha options
  {
    "goolord/alpha-nvim",
    opts = function(_, opts)
      -- customize the dashboard header
      opts.section.header.val = {
        " █████  ███████ ████████ ██████   ██████",
        "██   ██ ██         ██    ██   ██ ██    ██",
        "██   ██ ███████    ██    ██   ██  ██████",
        " ",
        "    ███    ██ ██    ██ ██ ███    ███",
        "    ████   ██ ██    ██ ██ ████  ████",
        "    ██   ████   ████   ██ ██      ██",
      }

      -- opts.section.buttons.val = {
      --   opts.button("y", "  Say YO CUSTOM BUTTON", ':echo "YO WHATS REALLY GOOD, WORLD?!"<CR>'),
      -- }
      return opts
    end,
  },

  -- You can disable default plugins as follows:
  -- { "max397574/better-escape.nvim", enabled = false },

  -- You can also easily customize additional setup of plugins that is outside of the plugin's setup call
  -- {
  --   "L3MON4D3/LuaSnip",
  --   config = function(plugin, opts)
  --     require "astronvim.plugins.configs.luasnip"(plugin, opts) -- include the default astronvim config that calls the setup call
  --     -- add more custom luasnip configuration such as filetype extend or custom snippets
  --     local luasnip = require "luasnip"
  --     luasnip.filetype_extend("javascript", { "javascriptreact" })
  --   end,
  -- },

  -- extending telescope
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { -- add a new dependency to telescope that is our new plugin
      "nvim-telescope/telescope-media-files.nvim",
    },
    -- the first parameter is the plugin specification
    -- the second is the table of options as set up in Lazy with the `opts` key
    config = function(plugin, opts)
      -- run the core AstroNvim configuration function with the options table
      require "astronvim.plugins.configs.telescope"(plugin, opts)

      -- require telescope and load extensions as necessary
      require("telescope").load_extension "media_files"
    end,
  },

  {
    "NeogitOrg/neogit",
    lazy = true, -- lazy load on module
    dependencies = {
      { -- AstroCore is always loaded on startup, so making it a dependency doesn't matter
        "AstroNvim/astrocore",
        opts = {
          mappings = { -- define a mapping to load the plugin module
            n = {
              ["gm"] = { function() require("neogit").open() end, desc = "Magit" },
            },
          },
        },
      },
    },
    opts = {}, -- run `require("neogit").setup({})`
  },

  {
    "danymat/neogen",
    cmd = "Neogen", -- lazy load on command
    dependencies = {
      { -- AstroCore is always loaded on startup, so making it a dependency doesn't matter
        "AstroNvim/astrocore",
        opts = {
          mappings = { -- define a mapping to invoke the command
            n = {
              -- ["<Leader>a"] = { function() vim.cmd "Neogen" end, desc = "Neogen" },
            },
          },
        },
      },
    },
    opts = {},
  },
}
