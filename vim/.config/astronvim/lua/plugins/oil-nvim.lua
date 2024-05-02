-- Customize Treesitter

---@type LazySpec
return {
  "stevearc/oil.nvim",
  opts = function(_, opts)
    -- add more things to the ensure_installed table protecting against community packs modifying it
    opts.ensure_installed = require("astrocore").list_insert_unique(opts.ensure_installed, {
      "lua",
      "vim",
      -- add more arguments for adding more treesitter parsers
    })
    opts.mappings = {
      n = {
        ["<Leader>o"] = { function() require("oil").open() end, desc = "Open folder in Oil" },
      },
    }
  end,
}
