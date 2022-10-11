-- Set the leader key.
vim.g.mapleader = " "

-- Toggle last buffer more easily.
vim.keymap.set('n', '<leader><leader>', '<C-6>')

-- Save and quit using simpler commands.
vim.keymap.set('n', '<C-S>', function() vim.cmd("write") end)
vim.keymap.set('i', '<C-S>', '<Esc>:write<CR>')
vim.keymap.set('n', '<C-Q>', function() vim.cmd("quit") end)
vim.keymap.set('n', '<C-G>x', function() vim.cmd("xit") end)
vim.keymap.set('n', 'gq', function() vim.cmd("bdelete") end)

-- Center the cursor after some mappings.
vim.keymap.set('n', '<C-T>', '<C-T>zz')
vim.keymap.set('n', '<C-O>', '<C-O>zz')
vim.keymap.set('n', '<C-I>', '<C-I>zz')

-- Replace awkward move to beginning of line.
vim.keymap.set('n', '<C-H>', 'H')
vim.keymap.set('n', 'H', '^')
vim.keymap.set('n', 'cH', 'c^')
vim.keymap.set('n', 'dH', 'd^')
vim.keymap.set('n', 'yH', 'y^')
vim.keymap.set('v', 'H', '^')

-- Replace awkward move to end of line.
vim.keymap.set('n', '<C-L>', 'L')
vim.keymap.set('n', 'L', '$')
vim.keymap.set('v', 'L', '$')
vim.keymap.set('n', 'yL', 'y$')

-- Copy and pasting from the system clipboard.
vim.keymap.set('n', 'gp', '"+pv`]=`>')
vim.keymap.set('n', 'gP', '"+Pv`]=`>')
vim.keymap.set('n', '=gp', 'o<Esc>"+pv`]=`>')
vim.keymap.set('n', '=gP', 'o<Esc>"+Pv`]=`>')
vim.keymap.set('n', 'gy', '"+y')
vim.keymap.set('v', 'gy', '"+y')

-- Fugitive bindings.
vim.keymap.set('n', 'gs', function () vim.cmd("G") end)
vim.keymap.set('n', 'gl', function () vim.cmd("Git log") end)
vim.keymap.set('n', 'gt', function ()
    vim.cmd('silent Ggrep! TODO(chlyz)')
    vim.cmd('copen')
end)
vim.keymap.set('n', '<C-G>s', function () vim.cmd("G") end)
vim.keymap.set('n', '<leader>gs', function () vim.cmd("G") end)
vim.keymap.set('n', '<leader>gb', function () vim.cmd("Git blame") end)
vim.keymap.set('n', '<leader>gm', function () vim.cmd("Gvdiffsplit dev/master") end)
vim.keymap.set('n', '<leader>gw', function ()
    local current_word = vim.call('expand', '<cword>')
    vim.cmd('silent Ggrep! ' .. current_word)
    vim.cmd.copen()
end)

-- Fuzzy search files and buffers.
vim.keymap.set('n', '<Leader>.', function()
    require'telescope.builtin'.git_files(require('telescope.themes').get_dropdown({
        layout_config = {
            width = 0.8,
            height = 0.5,
        },
    }))
end)
vim.keymap.set('n', '<Leader>>', function()
    require'telescope.builtin'.find_files(require('telescope.themes').get_dropdown({
        layout_config = {
            width = 0.8,
            height = 0.5,
        },
    }))
end)
vim.keymap.set('n', '<Leader>,', function()
    require'telescope.builtin'.buffers(require('telescope.themes').get_dropdown({
        layout_config = {
            width = 0.8,
            height = 0.5,
        },
    }))
end)
vim.keymap.set('n', '<Leader><', function()
    vim.cmd("Telescope")
end)
vim.keymap.set('n', '<Leader>;', function()
    require'telescope.builtin'.current_buffer_tags(require('telescope.themes').get_dropdown({
        layout_config = {
            width = 0.8,
            height = 0.5,
        },
    }))
end)
vim.keymap.set('n', '<Leader>:', function()
    require'telescope.builtin'.command_history(require('telescope.themes').get_dropdown({
        layout_config = {
            width = 0.8,
            height = 0.5,
        },
    }))
end)
vim.keymap.set('n', '<Leader>/', function()
    require'telescope.builtin'.live_grep(require('telescope.themes').get_dropdown({
        layout_config = {
            width = 0.8,
            height = 0.5,
        },
    }))
end)

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
vim.keymap.set('n', '<leader>dt', function() vim.cmd("windo diffthis") end)
vim.keymap.set('n', '<leader>do', function() vim.cmd("windo diffoff") end)

vim.keymap.set('n', '<leader>m', function () vim.cmd("Make") end)
vim.keymap.set('n', '<leader>as', function () vim.cmd("TSHighlightCapturesUnderCursor") end)


vim.keymap.set('n', '<leader>ck', function ()
    vim.cmd("edit /home/chlyz/.config/kitty/kitty.conf")
end)

vim.keymap.set('n', '<leader>cv', function ()
    vim.cmd("edit /home/chlyz/.config/nvim/init.lua")
end)

vim.keymap.set('n', '<leader>cs', function ()
    vim.cmd("source /home/chlyz/.config/nvim/init.lua")
end)
