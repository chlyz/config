vim.g.mapleader = vim.g.mapleader or " "

vim.pack.add({
  "https://github.com/tpope/vim-fugitive",
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
