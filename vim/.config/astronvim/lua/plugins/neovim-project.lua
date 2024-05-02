return {
  "coffebar/neovim-project",
  opts = {
    projects = { -- define project roots
      "~/projects/*",
      "~/.config/*",
    },
  },
  init = function()
    -- enable saving the state of plugins in the session
    ---    vim.opt.sessionoptions:append "globals" -- save global variables that start with an uppercase letter and contain at least one lowercase letter.
  end,
  dependencies = {
    { "nvim-lua/plenary.nvim" },
    { "nvim-telescope/telescope.nvim", tag = "0.1.4" },
    { "Shatur/neovim-session-manager" },
  },
  lazy = false,
  priority = 100,
  keys = {
    {
      "<Leader>Pd",
      function() vim.cmd "Telescope neovim-project discover" end,
      desc = "Project discover",
    },
    {
      "<Leader>Ph",
      function() vim.cmd "Telescope neovim-project history" end,
      desc = "Project history",
    },
  },
}
