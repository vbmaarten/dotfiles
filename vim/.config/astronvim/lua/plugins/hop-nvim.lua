return {
  "smoka7/hop.nvim",
  version = "*",
  opts = {
    keys = "asdfjkl",
  },
  keys = {
    {
      "<Leader>a",
      function() vim.cmd "HopWord" end,
      desc = "Hop word",
    },
  },
}
