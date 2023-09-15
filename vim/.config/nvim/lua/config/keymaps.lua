-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
local Util = require("lazyvim.util")

function getVisualSelection()
  local modeInfo = vim.api.nvim_get_mode()
  local mode = modeInfo.mode

  local cursor = vim.api.nvim_win_get_cursor(0)
  local cline, ccol = cursor[1], cursor[2]
  local vline, vcol = vim.fn.line("v"), vim.fn.col("v")

  local sline, scol
  local eline, ecol
  if cline == vline then
    if ccol <= vcol then
      sline, scol = cline, ccol
      eline, ecol = vline, vcol
      scol = scol + 1
    else
      sline, scol = vline, vcol
      eline, ecol = cline, ccol
      ecol = ecol + 1
    end
  elseif cline < vline then
    sline, scol = cline, ccol
    eline, ecol = vline, vcol
    scol = scol + 1
  else
    sline, scol = vline, vcol
    eline, ecol = cline, ccol
    ecol = ecol + 1
  end

  if mode == "V" or mode == "CTRL-V" or mode == "\22" then
    scol = 1
    ecol = nil
  end

  local lines = vim.api.nvim_buf_get_lines(0, sline - 1, eline, 0)
  if #lines == 0 then
    return
  end

  local startText, endText
  if #lines == 1 then
    startText = string.sub(lines[1], scol, ecol)
  else
    startText = string.sub(lines[1], scol)
    endText = string.sub(lines[#lines], 1, ecol)
  end

  local selection = { startText }
  if #lines > 2 then
    vim.list_extend(selection, vim.list_slice(lines, 2, #lines - 1))
  end
  table.insert(selection, endText)

  return selection
end

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

map("n", "<leader>mm", function()
  Util.float_term({ "spt" }, { cwd = Util.get_root(), esc_esc = false, ctrl_hjkl = false, border = "single" })
end, { desc = "Spotify UI" })

map("n", "<leader>mi", function()
  Util.float_term({ "spotd" }, { cwd = Util.get_root(), esc_esc = false, ctrl_hjkl = false, border = "single" })
end, { desc = "Spotify device initialization" })

map("n", "<leader>mt", function()
  local handle = io.popen("spt playback -t")
  local result = handle:read("*a")
  handle:close()
  vim.notify(result)
end, { desc = "Spotify playback toggle" })

map("n", "<leader>mp", function()
  local handle = io.popen("spt playback -p")
  local result = handle:read("*a")
  handle:close()
  vim.notify(result)
end, { desc = "Spotify previous" })

map("n", "<leader>mn", function()
  local handle = io.popen("spt playback -n")
  local result = handle:read("*a")
  handle:close()
  vim.notify(result)
end, { desc = "Spotify next" })

map("n", "<leader>mc", function()
  local handle = io.popen("spt playback -s")
  local result = handle:read("*a")
  handle:close()
  vim.notify(result)
end, { desc = "Spotify show current song" })

map("n", "<leader>si", ":Telescope simulators run<CR>")

map("v", "<leader>wx", ":'<,'>!bash<CR>", { desc = "Execute visual selection" })

map("n", "<leader>id", ':r! date "+\\%Y-\\%m-\\%d" <CR>', { desc = "Insert date" })
map("n", "<leader>wc", ":r!pandoc -f vimwiki -t markdown '%:p' <CR>")

-- Vimwiki extra keymaps
map("n", "<leader>wa", ":VimwikiAll2HTML <CR>", { desc = "Vimwiki: export HTML" })
map("n", "<leader>wb", ":Vimwiki2HTMLBrowse <CR>", { desc = "Vimwiki: browse HTML" })
map("n", "<leader>wp", function()
  io.popen("cd ~/vimwiki && ./push.sh > /dev/null")
  vim.notify("Changes pushed, remember your password!")
end, { desc = "Vimwiki: push changes" })

map("n", "<leader>uts", ":TomatoStart <CR>", { desc = "Pomodoro start" })
map("n", "<leader>ute", ":TomatoStop <CR>", { desc = "Pomodoro end" })

map("n", "<leader>uh", function()
  Util.float_term({ "hnt" }, { cwd = Util.get_root(), esc_esc = false, ctrl_hjkl = false, border = "single" })
end, { desc = "Hacker news" })

map("n", "<leader>t", function()
  Util.float_term(
    { "taskwarrior-tui" },
    { cwd = Util.get_root(), esc_esc = false, ctrl_hjkl = false, border = "single" }
  )
end, { desc = "Taskwarrior" })
