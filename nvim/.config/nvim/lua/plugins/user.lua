-- You can also add or configure plugins by creating files in this `plugins/` folder
-- Here are some examples:

---@type LazySpec
return {

  -- ctrl-p
  -- (originally from https://github.com/twinlock/dotfiles/blob/9fc1576c7eccd4494db14f83ed7ad3ff9fb3def5/config/nvim/lua/plugins.lua#L62)
  {
    "ctrlpvim/ctrlp.vim",
    config = function()
      vim.cmd [[
        let g:ctrlp_match_window = 'order:ttb,max:20'
        " dont serch but every 250ms, eliminates some annoying fumble finger behavior
        let g:ctrlp_lazy_update = 150
        let g:ctrlp_working_path_mode = 'ra'
        let g:ctrlp_working_path_mode = 0
        " Regex mode by default (<c-r> to toggle)
        let g:ctrlp_regexp = 0
        let g:ctrlp_custom_ignore = {
                      \ 'dir':  '\.git$\|\.hg$\|\.svn$|\.pants.d$',
                      \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$|\.swp$' }
        if executable('ag')
          let s:ctrlp_fallback = 'ag %s --nocolor -l -g ""'
        else
          let s:ctrlp_fallback = 'find %s -type f'
        endif
        let g:ctrlp_user_command = {
                      \ 'types': {
                      \ 1: ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others'],
                      \ 2: ['.hg', 'hg --cwd %s locate -I .'],
                      \ },
                      \ 'fallback': s:ctrlp_fallback
                      \ }
      ]]
    end,

    -- keymap('n', "<leader>ppp", ":CtrlPMixed<CR>", opts)
    -- keymap('n', "<leader>ppt", ":CtrlPTags<CR>", opts)
    -- keymap('n', "<leader>pph", ":CtrlPMRUFiles<CR>", opts)
    dependencies = {
      { -- AstroCore is always loaded on startup, so making it a dependency doesn't matter
        "AstroNvim/astrocore",
        opts = {
          mappings = { -- define a mapping to load the plugin module
            n = {
              ["<leader>p"] = { ":CtrlP<CR>", desc = "Ctrl-p files" },
              ["<leader>P"] = { ":CtrlPClearAllCaches<CR>", desc = "Ctrl-p cache clear" },
              ["<leader>bb"] = { ":CtrlPBuffer<CR>", desc = "Ctrl-p buffers" },
              ["<leader>bm"] = { ":CtrlPMixed<CR>", desc = "Ctrl-p mixed" },
              ["<leader>bf"] = { ":CtrlPMRUFiles<CR>", desc = "Ctrl-p MRU files" },
            },
          },
        },
      },
    },
  },

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
      "nvim-telescope/telescope-project.nvim",
    },
    -- the first parameter is the plugin specification
    -- the second is the table of options as set up in Lazy with the `opts` key
    config = function(plugin, opts)
      -- run the core AstroNvim configuration function with the options table
      require "astronvim.plugins.configs.telescope"(plugin, opts)

      -- require telescope and load extensions as necessary
      require("telescope").load_extension "media_files"
      require("telescope").load_extension "project"
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
  },
}
