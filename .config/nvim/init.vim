
" Built-in settings {{{
set number
set relativenumber
set nowrap
set mouse=a
set ignorecase
set smartcase
set expandtab
set shiftwidth=4
set softtabstop=4
set smartindent
set termguicolors
set cursorline
set guicursor=n-c-v:blinkwait300-blinkon200-blinkoff150,i-ci-ve:blinkon1-ver25,r-cr-o:hor20-blinkon1
" }}}

" Plugins {{{
call plug#begin()

Plug 'nvim-lua/plenary.nvim'

Plug 'chriskempson/base16-vim'

Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'

Plug 'junegunn/fzf', { 'dir': '~/.local/git/fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/playground'

Plug 'ThePrimeagen/harpoon'

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'ludovicchabant/vim-gutentags'

" Plug 'ap/vim-css-color'
Plug 'chrisbra/Colorizer'
Plug  'gerw/vim-HiLinkTrace'

call plug#end()
" }}}

let g:gutentags_ctags_exclude = [
            \ '*bazel-*', '.git', 'Build', '.ccls-cache', 'MDF4', 'HDF5',
            \ 'Compiler', 'sqlite3.*', 'Rte_*.*', 'VehicleModel', 'ProtoBuf',
            \ 'Protobuf', '*.cs'
            \]
let g:fzf_tags_command = 'ctags -R --exclude=.git --exclude=Build
            \ --exclude=.ccls-cache --exclude=Rte_*.* --exclude=Protobuf
            \ --exclude=ProtoBuf --exclude=.ccls-cache --exclude=*bazel*
            \ --languages=-javascript'

" Functions {{{
" Return to last edit position when opening files.
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif

" Highlight yanked text.
augroup highlight_yank
    autocmd!
    autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank({timeout = 100})
augroup END

function! ToggleQuickFix()
    if empty(filter(getwininfo(), 'v:val.quickfix'))
        copen
    else
        cclose
    endif
endfunction
" }}}

" Mappings {{{
let mapleader = " "

nnoremap <leader>n :nohlsearch<CR>

nnoremap <leader>. :GFiles<CR>
nnoremap <leader>, :Buffers<CR>
nnoremap <leader>p :Files<CR>
nnoremap <leader>; :History:<CR>
nnoremap <leader>/ :History/<CR>
nnoremap <leader>' :BTags<CR>

nnoremap <leader>ua :lua require("harpoon.mark").add_file()<CR>
nnoremap <leader>um :lua require("harpoon.ui").toggle_quick_menu()<CR>
nnoremap <leader>uh :lua require("harpoon.ui").nav_file(1)<CR>
nnoremap <leader>ut :lua require("harpoon.ui").nav_file(2)<CR>
nnoremap <leader>un :lua require("harpoon.ui").nav_file(3)<CR>
nnoremap <leader>us :lua require("harpoon.ui").nav_file(4)<CR>

nnoremap <silent> <leader>gs :G<CR>
nnoremap <silent> <leader>gw :silent Ggrep! <C-R><C-W><CR>:copen<CR>
nnoremap <leader>gb :Git blame<CR>
nnoremap <leader>gd :Gvdiffsplit<CR>

nnoremap <leader>do :windo diffthis<CR>
nnoremap <leader>dd :windo diffoff<CR>

nnoremap <silent> <leader>cc :call ToggleQuickFix()<CR>
nnoremap <silent> <leader>cs :source ~/.config/nvim/init.vim<CR>
nnoremap <silent> <leader>cv :edit ~/.config/nvim/init.vim<CR>

nnoremap <leader>y "+y
vnoremap <leader>y "+y
nnoremap <leader>p "+p
nnoremap <leader>P "+P

nnoremap <silent> <leader>sc :w<CR>:so<CR>

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
vmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
" }}}

" Harpoon {{{
lua << EOF
require("harpoon").setup({
    menu = {
        width = 120,
    }
})
EOF
" }}}

lua << EOF
require'nvim-treesitter.configs'.setup({
  -- One of "all", "maintained" (parsers with maintainers), or a list of languages
  ensure_installed = "maintained",

  -- Install languages synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- List of parsers to ignore installing
  ignore_install = { "javascript" },

  highlight = {
    -- `false` will disable the whole extension
    enable = true,

    -- NOTE: these are the names of the parsers and not the filetype. (for example if you want to disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is the name of the parser)
    -- list of language that will be disabled
    disable = { },

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
})
EOF
abbreviate td TODO(chlyz):

" let base16colorspace=256
" colorscheme base16-gruvbox-light-hard

colorscheme modus-operandi
" colorscheme modus-vivendi

" Show syntax highlighting groups for word under cursor
" nmap <C-P> :HLT<CR>
nnoremap <C-P> :TSHighlightCapturesUnderCursor<CR>
