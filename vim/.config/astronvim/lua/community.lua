-- AstroCommunity: import any community modules here
-- We import this file in `lazy_setup.lua` before the `plugins/` folder.
-- This guarantees that the specs are processed before any user plugins.

---@type LazySpec
return {
  "AstroNvim/astrocommunity",
  { import = "astrocommunity.pack.lua" },
  { import = "astrocommunity.pack.typescript" },
  { import = "astrocommunity.pack.typescript-all-in-one" },
  --{ import = "astrocommunity.motion.harpoon" },
  { import = "astrocommunity.motion.marks-nvim" },
  { import = "astrocommunity.file-explorer.oil-nvim" },
  { import = "astrocommunity.git.diffview-nvim" },
  { import = "astrocommunity.git.neogit" },
  { import = "astrocommunity.terminal-integration.flatten-nvim" },
  --  { import = "astrocommunity.completion.copilot-lua-cmp" },
  { import = "astrocommunity.project.nvim-spectre" },
  { import = "astrocommunity.keybinding.nvcheatsheet-nvim" },
  { import = "astrocommunity.code-runner.sniprun" },
  { import = "astrocommunity.code-runner.toggletasks-nvim" },
  { import = "astrocommunity.utility.noice-nvim" },
  -- import/override with your plugins folder
}
