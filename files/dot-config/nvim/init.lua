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
  "https://github.com/nvim-treesitter/nvim-treesitter",
  "https://github.com/tpope/vim-fugitive",
})

require("nvim-treesitter").install({ "markdown" })

vim.api.nvim_create_autocmd("FileType", {
  pattern = "markdown",
  callback = function(args)
    pcall(vim.treesitter.start, args.buf)
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

local telescope = require("telescope.builtin")

vim.keymap.set("n", "<leader>.", telescope.git_files, {
  desc = "Find Git files",
})

vim.keymap.set("n", "<leader>,", telescope.buffers, {
  desc = "Switch buffer",
})

vim.keymap.set("n", "<leader>gs", "<cmd>Git<cr>", {
  desc = "Fugitive status",
})

vim.keymap.set("n", "<leader>gm", "<cmd>Gvdiffsplit! master<cr>", {
  desc = "Diff current file against master",
})

vim.opt.termguicolors = true
vim.opt.background = "dark"
vim.cmd.colorscheme("modus_vivendi_tinted")
