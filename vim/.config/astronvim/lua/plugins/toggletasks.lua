return {
  "jedrzejboczar/toggletasks.nvim",
  dependencies = {
    { "nvim-lua/plenary.nvim" },
    { "akinsho/toggleterm.nvim" },
    { "nvim-telescope/telescope.nvim/" },
  },
  config = function()
    require("toggletasks").setup {
      telescope = {
        spawn = {
          open_single = true, -- auto-open terminal window when spawning a single task
          show_running = false, -- include already running tasks in picker candidates
          -- Replaces default select_* actions to spawn task (and change toggleterm
          -- direction for select horiz/vert/tab)
          mappings = {
            select_float = "<C-f>",
            spawn_smart = "<C-a>", -- all if no entries selected, else use multi-select
            spawn_all = "<M-a>", -- all visible entries
            spawn_selected = nil, -- entries selected via multi-select (default <tab>)
          },
        },
        -- Replaces default select_* actions to open task terminal (and change toggleterm
        -- direction for select horiz/vert/tab)
        select = {
          mappings = {
            select_float = "<C-f>",
            open_smart = "<C-a>",
            open_all = "<M-a>",
            open_selected = nil,
            kill_smart = nil,
            kill_all = "<M-q>",
            kill_selected = "<C-q>",
            respawn_smart = "<C-s>",
            respawn_all = "<M-s>",
            respawn_selected = nil,
          },
        },
      },
    }
    require("telescope").load_extension "toggletasks"
  end,
  keys = {
    { "<leader>tss", "<cmd>Telescope toggletasks spawn<CR>", desc = "toggletasks: spawn" },
    { "<leader>tsc", "<cmd>Telescope toggletasks select<CR>", desc = "toggletasks: select" },
    { "<leader>tse", "<cmd>Telescope toggletasks edit<CR>", desc = "toggletasks: edit config" },
  },
}
