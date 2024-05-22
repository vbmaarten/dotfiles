return {
  "nvim-telescope/telescope-live-grep-args.nvim",
  dependencies = {
    {
      "nvim-telescope/telescope-live-grep-args.nvim",
      -- This will not install any breaking changes.
      -- For major updates, this must be adjusted manually.
      version = "^1.0.0",
    },
  },
  config = function()
    local telescope = require "telescope"
    telescope.load_extension "live_grep_args"
  end,
  keys = {
    {
      "<Leader>fg",
      function() require("telescope").extensions.live_grep_args.live_grep_args() end,
      desc = "Find word with arguments",
    },
  },
}
