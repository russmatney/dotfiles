-- Plugin funcs
local function plugins_install() require("lazy").install() end
local function plugins_status() require("lazy").home() end
local function plugins_sync() require("lazy").sync() end
local function plugins_check_updates() require("lazy").check() end
local function plugins_update() require("lazy").update() end
local function plugins_update_packages() require("astrocore").update_packages() end

local function hover_diagnostics() vim.diagnostic.open_float() end

local cmds = {
  { "PluginsInstall", plugins_install, {} },
  { "PluginsStatus", plugins_status, {} },
  { "PluginsSync", plugins_sync, {} },
  { "PluginsCheckUpdates", plugins_check_updates, {} },
  { "PluginsUpdate", plugins_update, {} },
  { "PluginsUpdatePackages", plugins_update_packages, {} },
  { "HoverDiagnostics", hover_diagnostics, {} },
}

for _i, cmd in pairs(cmds) do
  vim.api.nvim_create_user_command(cmd[1], cmd[2], cmd[3])
end

---@type LazySpec
return {
  -- treesitter
  { "nvim-treesitter/nvim-treesitter", opts = {
    ensure_installed = { "lua", "vim" },
  } },

  -- wakatime!!
  { "wakatime/vim-wakatime", lazy = false },
  -- magit
  { "NeogitOrg/neogit", lazy = true },

  -- telescope
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
  },
}
