-- AstroCommunity: import any community modules here
-- We import this file in `lazy_setup.lua` before the `plugins/` folder.
-- This guarantees that the specs are processed before any user plugins.

---@type LazySpec
return {
  "AstroNvim/astrocommunity",
  { import = "astrocommunity.pack.lua" },

  -- astro behavior
  { import = "astrocommunity.recipes.astrolsp-no-insert-inlay-hints" },
  { import = "astrocommunity.recipes.cache-colorscheme" },

  -- langs
  { import = "astrocommunity.pack.lua" },
  { import = "astrocommunity.pack.typescript" },
  { import = "astrocommunity.pack.cpp" },
  { import = "astrocommunity.pack.rust" },
  { import = "astrocommunity.pack.java" },

  -- themes
  { import = "astrocommunity.colorscheme.catppuccin" },

  -- overwritten in lua/plugins/*
  -- vscode
  -- { import = "astrocommunity.recipes.vscode" },
  -- neovide
  -- { import = "astrocommunity.recipes.neovide" },
}
