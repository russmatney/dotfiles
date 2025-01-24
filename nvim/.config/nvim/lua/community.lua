-- AstroCommunity: import any community modules here
-- We import this file in `lazy_setup.lua` before the `plugins/` folder.
-- This guarantees that the specs are processed before any user plugins.

---@type LazySpec
return {
  "AstroNvim/astrocommunity",

  -- astro behavior
  { import = "astrocommunity.recipes.astrolsp-no-insert-inlay-hints" },
  { import = "astrocommunity.recipes.cache-colorscheme" },
  { import = "astrocommunity.recipes.telescope-lsp-mappings" },
  -- vscode
  -- { import = "astrocommunity.recipes.vscode" },
  -- neovide
  { import = "astrocommunity.recipes.neovide" },

  -- langs
  { import = "astrocommunity.pack.lua" },
  { import = "astrocommunity.pack.typescript" },
  -- { import = "astrocommunity.pack.cs" },
  -- { import = "astrocommunity.pack.cs-omnisharp" },
  { import = "astrocommunity.pack.cpp" },
  { import = "astrocommunity.pack.rust" },
  { import = "astrocommunity.pack.java" },

  -- themes
  { import = "astrocommunity.colorscheme.catppuccin" },
}
