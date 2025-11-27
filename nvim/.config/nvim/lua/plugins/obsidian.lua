vim.o.conceallevel = 2

return {
  "obsidian-nvim/obsidian.nvim",
  version = "*", -- recommended, use latest release instead of latest commit
  ft = "markdown",
  -- Replace the above line with this if you only want to load obsidian.nvim for markdown files in your vault:
  -- event = {
  -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
  -- E.g. "BufReadPre " .. vim.fn.expand "~" .. "/my-vault/*.md"
  -- refer to `:h file-pattern` for more examples
  --   "BufReadPre "
  --     .. vim.fn.expand "~"
  --     .. "/MWEB Notes/*/**.md",
  --   "BufNewFile " .. vim.fn.expand "~" .. "/MWEB Notes/*/**.md",
  --   "BufReadPre path/to/my-vault/*.md",
  --   "BufNewFile path/to/my-vault/*.md",
  -- },
  ---@module 'obsidian'
  ---@type obsidian.config
  opts = {
    legacy_commands = false,
    note_id_func = function(title) return title end,
    notes_subdir = "inbox",
    new_notes_location = "notes_subdir",
    open_notes_in = "vsplit",
    daily_notes = {
      folder = "daily",
    },
    workspaces = {
      {
        name = "danger russ vault",
        path = "~/Documents/Danger Russ Remote Vault",
      },
    },
    frontmatter = {
      enabled = false,
    },
  },
}
