-- Only required if you have packer configured as `opt`
-- vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
    use 'wbthomason/packer.nvim'
    use 'lewis6991/impatient.nvim'

    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
    }
    use 'nvim-treesitter/playground'

    use 'tpope/vim-dispatch'
    use 'tpope/vim-eunuch'
    use 'tpope/vim-fugitive'
    use 'tpope/vim-repeat'
    use 'tpope/vim-surround'
    use 'tpope/vim-unimpaired'

    -- File browser
    use 'justinmk/vim-dirvish'

    use "TimUntersberger/neogit"

    -- Completion packages.
    -- use 'hrsh7th/cmp-nvim-lsp'
    use 'hrsh7th/cmp-buffer'
    use 'hrsh7th/cmp-path'
    use 'hrsh7th/cmp-cmdline'
    use 'hrsh7th/nvim-cmp'
    -- use 'quangnguyen30192/cmp-nvim-tags'

    use 'nvim-lualine/lualine.nvim'

    use 'wincent/loupe'

    use 'dag/vim-fish'

    use {
        'nvim-telescope/telescope.nvim',
        requires = { {'nvim-lua/plenary.nvim'} }
    }

    use {
        'nvim-telescope/telescope-fzf-native.nvim',
        run = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build'
    }

    use {
      "antoinemadec/FixCursorHold.nvim",
      run = function()
        vim.g.curshold_updatime = 1000
      end,
    }

    use 'numToStr/Comment.nvim'

    use 'folke/tokyonight.nvim'

    use 'ludovicchabant/vim-gutentags'

    use 'ThePrimeagen/harpoon'

    use {
        'glts/vim-textobj-comment',
        requires = { {'kana/vim-textobj-user'} }
    }

end)
