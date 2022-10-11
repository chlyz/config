-- Set the leader key.
vim.g.mapleader = " "

-- Toggle last buffer more easily.
vim.keymap.set('n', '<leader><leader>', '<C-6>')

-- Save and quit using simpler commands.
vim.keymap.set('n', '<C-S>', ':write<CR>')
vim.keymap.set('i', '<C-S>', '<Esc>:write<CR>')
vim.keymap.set('n', '<C-Q>', ':quit<CR>')
vim.keymap.set('n', 'gq',    ':bdelete<CR>')

-- Center the cursor after some mappings.
vim.keymap.set('n', '<C-T>', '<C-T>zz')
vim.keymap.set('n', '<C-O>', '<C-O>zz')
vim.keymap.set('n', '<C-I>', '<C-I>zz')

-- Copy and pasting from the system clipboard.
vim.keymap.set('n', 'gp', '"+pv`]=`>')
vim.keymap.set('n', 'gP', '"+Pv`]=`>')
vim.keymap.set('n', '=gp', 'o<Esc>"+pv`]=`>')
vim.keymap.set('n', '=gP', 'o<Esc>"+Pv`]=`>')
vim.keymap.set('n', 'gy', '"+y')
vim.keymap.set('v', 'gy', '"+y')

-- Fugitive bindings.
vim.keymap.set('n', 'gs', ':G<CR>')
vim.keymap.set('n', 'gl', ':Git log<CR>')
vim.keymap.set('n', '<Leader>gT', function ()
    vim.cmd('silent Ggrep! TODO')
    vim.cmd('copen')
end)
vim.keymap.set('n', '<Leader>gt', function ()
    vim.cmd('silent Ggrep! TODO(chlyz)')
    vim.cmd('copen')
end)
vim.keymap.set('n', '<leader>gb', ':Git blame<CR>')
vim.keymap.set('n', '<leader>gm', ':Gvdiffsplit dev/master')
vim.keymap.set('n', '<leader>gw', function ()
    local current_word = vim.call('expand', '<cword>')
    vim.cmd('silent Ggrep! ' .. current_word)
    vim.cmd.copen()
end)

-- Fuzzy search files and buffers.
vim.keymap.set('n', '<C-h>',     ':Telescope help_tags<CR>')
vim.keymap.set('n', '<Leader>.', ':Telescope git_files<CR>')
vim.keymap.set('n', '<Leader>>', ':Telescope find_files<CR>')
vim.keymap.set('n', '<Leader>,', ':Telescope buffers<CR>')
vim.keymap.set('n', '<Leader><', ':Telescope builtin<CR>')
vim.keymap.set('n', '<Leader>;', ':Telescope current_buffer_tags<CR>')
vim.keymap.set('n', '<Leader>:', ':Telescope command_history<CR>')
vim.keymap.set('n', '<Leader>/', ':Telescope live_grep<CR>')

-- Harpoon for fast and "consistent" file access.
vim.keymap.set('n', '<leader>aa', function() require("harpoon.mark").add_file() end)
vim.keymap.set('n', '<leader>ac', function() require("harpoon.mark").clear_all() end)
vim.keymap.set('n', '<leader>ar', function() require("harpoon.mark").rm_file() end)
vim.keymap.set('n', '<leader>am', function() require("harpoon.ui").toggle_quick_menu() end)
vim.keymap.set('n', '<leader>ah', function() require("harpoon.ui").nav_file(1) end)
vim.keymap.set('n', '<leader>at', function() require("harpoon.ui").nav_file(2) end)
vim.keymap.set('n', '<leader>an', function() require("harpoon.ui").nav_file(3) end)
vim.keymap.set('n', '<leader>as', function() require("harpoon.ui").nav_file(4) end)
vim.keymap.set('n', '<M-g>', function() require("harpoon.ui").nav_file(1) end)
vim.keymap.set('n', '<M-c>', function() require("harpoon.ui").nav_file(2) end)
vim.keymap.set('n', '<M-r>', function() require("harpoon.ui").nav_file(3) end)
vim.keymap.set('n', '<M-l>', function() require("harpoon.ui").nav_file(4) end)

-- TODO: Everything from here on needs some organization and commenting.
vim.keymap.set('n', '<leader>dt', ':windo diffthis<CR>')
vim.keymap.set('n', '<leader>do', ':windo diffoff<CR>')

vim.keymap.set('n', '<leader>m',  ':Make')
vim.keymap.set('n', '<leader>as', ':TSHighlightCapturesUnderCursor')


vim.keymap.set('n', '<leader>ck', function ()
    vim.cmd("edit /home/chlyz/.config/kitty/kitty.conf")
end)

vim.keymap.set('n', '<leader>cv', function ()
    vim.cmd("edit /home/chlyz/.config/nvim/init.lua")
end)

vim.keymap.set('n', '<leader>cs', function ()
    vim.cmd("source /home/chlyz/.config/nvim/init.lua")
end)
