vim.g.mapleader = vim.g.mapleader or " "

vim.api.nvim_create_autocmd("PackChanged", {
  callback = function(event)
    if event.data.kind == "update" and event.data.spec.name == "nvim-treesitter" then
      if not event.data.active then
        vim.cmd.packadd("nvim-treesitter")
      end
      require("nvim-treesitter").update({ "markdown" })
    elseif event.data.spec.name == "telescope-fzf-native.nvim"
      and (event.data.kind == "install" or event.data.kind == "update")
    then
      local result = vim.system({ "make" }, { cwd = event.data.path }):wait()
      if result.code ~= 0 then
        error("Could not build telescope-fzf-native.nvim: " .. result.stderr)
      end
    end
  end,
})

vim.pack.add({
  "https://github.com/nvim-lua/plenary.nvim",
  "https://github.com/nvim-telescope/telescope.nvim",
  "https://github.com/nvim-telescope/telescope-fzf-native.nvim",
  "https://github.com/crispgm/telescope-heading.nvim",
  "https://github.com/nvim-treesitter/nvim-treesitter",
  "https://github.com/tpope/vim-fugitive",
  "https://github.com/stevearc/oil.nvim",
})

require("nvim-treesitter").install({ "markdown" })

require("oil").setup()

vim.api.nvim_create_autocmd("FileType", {
  pattern = "markdown",
  callback = function(args)
    pcall(vim.treesitter.start, args.buf)
  end,
})

vim.api.nvim_create_autocmd("BufReadPost", {
  callback = function(args)
    if vim.bo[args.buf].buftype ~= "" then
      return
    end

    local position = vim.api.nvim_buf_get_mark(args.buf, '"')
    if position[1] > 0 and position[1] <= vim.api.nvim_buf_line_count(args.buf) then
      vim.api.nvim_win_set_cursor(0, position)
    end
  end,
})

require("telescope").setup({
  defaults = {
    border = true,
    borderchars = {
      prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
      results = { " " },
      preview = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
    },
    layout_config = { height = 13 },
    layout_strategy = "bottom_pane",
    preview = false,
    sorting_strategy = "ascending",
  },
  extensions = {
    fzf = {
      case_mode = "smart_case",
      fuzzy = true,
      override_file_sorter = true,
      override_generic_sorter = true,
    },
  },
})

require("telescope").load_extension("fzf")
require("telescope").load_extension("heading")

local telescope = require("telescope.builtin")

vim.keymap.set("n", "<leader>.", telescope.git_files, {
  desc = "Find Git files",
})

vim.keymap.set("n", "<leader>,", telescope.buffers, {
  desc = "Switch buffer",
})

vim.keymap.set("n", "<leader>'", function()
  if vim.bo.filetype == "markdown" then
    vim.cmd("Telescope heading")
  else
    telescope.treesitter()
  end
end, {
  desc = "Jump to heading or symbol",
})

vim.keymap.set("n", "-", "<cmd>Oil<cr>", {
  desc = "Browse parent directory",
})

vim.keymap.set("n", "<leader>gs", "<cmd>Git<cr>", {
  desc = "Fugitive status",
})

vim.keymap.set("n", "<leader>gm", "<cmd>Gvdiffsplit! master<cr>", {
  desc = "Diff current file against master",
})

vim.opt.termguicolors = true
vim.opt.background = "dark"

-- Terminal Modus supplies the shared generated Operandi/Vivendi schemes.
-- The local tinted scheme remains available until that variant is generated.
vim.opt.runtimepath:prepend(vim.fn.expand("~/config/vendor/terminal-modus"))

local selected_theme = "modus_vivendi_tinted"
local theme_state = io.open(vim.fn.expand("~/.config/modus-theme/current"), "r")
if theme_state then
  selected_theme = vim.trim(theme_state:read("*a"))
  theme_state:close()
end

local colorscheme = selected_theme:gsub("-", "_")
local available = vim.fn.getcompletion(colorscheme, "color")
if not vim.tbl_contains(available, colorscheme) then
  colorscheme = vim.startswith(selected_theme, "modus-operandi")
      and "modus_operandi"
    or "modus_vivendi"
end
vim.cmd.colorscheme(colorscheme)
