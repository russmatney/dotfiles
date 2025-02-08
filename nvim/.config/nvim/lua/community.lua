-- Astro Community modules from https://github.com/AstroNvim/astrocommunity/
--
-- This file should be imported before the `plugins/` folder.
-- This guarantees that the specs are processed before any user plugins.

---@type LazySpec
return {
  "AstroNvim/astrocommunity",

  -- astro behavior
  { import = "astrocommunity.recipes.astrolsp-no-insert-inlay-hints" },
  { import = "astrocommunity.recipes.cache-colorscheme" },
  { import = "astrocommunity.recipes.telescope-lsp-mappings" },

  -- langs
  { import = "astrocommunity.pack.lua" },
  { import = "astrocommunity.pack.typescript" },
  -- { import = "astrocommunity.pack.cs" },
  { import = "astrocommunity.pack.cs-omnisharp" },
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
