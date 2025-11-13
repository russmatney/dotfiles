return {}
-- return { -- override nvim-cmp plugin
--   "hrsh7th/nvim-cmp",
--   -- override the options table that is used in the `require("cmp").setup()` call
--   opts = function(_, opts)
--     local cmp = require "cmp"

--     opts.mapping["<C-j>"] = cmp.mapping.select_next_item()
--     opts.mapping["<C-k>"] = cmp.mapping.select_prev_item()
--     opts.mapping["<C-l>"] = cmp.mapping.complete()
--     opts.mapping["<Tab>"] = cmp.mapping.complete()

--     opts.sources = cmp.config.sources {
--       { name = "nvim_lsp", priority = 1000 },
--       { name = "luasnip", priority = 750 },
--       { name = "buffer", priority = 500 },
--       { name = "path", priority = 250 },
--     }
--   end,
-- }
