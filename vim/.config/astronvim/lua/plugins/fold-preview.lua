if true then return {} end

---@type LazySpec
return {
  "anuvyklack/fold-preview.nvim",
  dependencies = {
    { "anuvyklack/keymap-amend.nvim" },
    {
      "AstroNvim/astrocore",
      opts = {
        mappings = {
          n = {
            ["zh"] = function() require("fold-preview").close_preview() end,
          },
        },
      },
    },
  },
  opts = { auto = 400, default_keybindings = false },
}
