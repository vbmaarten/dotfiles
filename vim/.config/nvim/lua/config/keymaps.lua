-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
local Util = require("lazyvim.util")

local function map(mode, lhs, rhs, opts)
  local keys = require("lazy.core.handler").handlers.keys
  ---@cast keys LazyKeysHandler
  -- do not create the keymap if a lazy keys handler exists
  if not keys.active[keys.parse({ lhs, mode = mode }).id] then
    opts = opts or {}
    opts.silent = opts.silent ~= false
    if opts.remap and not vim.g.vscode then
      opts.remap = nil
    end
    vim.keymap.set(mode, lhs, rhs, opts)
  end
end

map("n", "<leader>mu", function()
  Util.float_term({ "spt" }, { cwd = Util.get_root(), esc_esc = false, ctrl_hjkl = false })
end, { desc = "Spotify UI" })

map("n", "<leader>mi", function()
  Util.float_term({ "spotd" }, { cwd = Util.get_root(), esc_esc = false, ctrl_hjkl = false })
end, { desc = "Spotify device initialization" })

map("n", "<leader>mt", function()
  local handle = io.popen("spt playback -t")
  local result = handle:read("*a")
  handle:close()
  vim.notify(result)
end, { desc = "Spotify playback toggle" })

map("n", "<leader>mc", function()
  local handle = io.popen("spt playback -s")
  local result = handle:read("*a")
  handle:close()
  vim.notify(result)
end, { desc = "Spotify show current song" })

map("n", "<leader>id", ':r! date "+\\%Y-\\%m-\\%d" <CR>', { desc = "Insert date" })

map("n", "<leader>uh", function()
  Util.float_term({ "hnt" }, { cwd = Util.get_root(), esc_esc = false, ctrl_hjkl = false })
end, { desc = "Hacker news" })
