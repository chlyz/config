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

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'

Plug 'ThePrimeagen/harpoon'

Plug 'neoclide/coc.nvim', {'branch': 'release'}

call plug#end()
" }}}

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

abbreviate td TODO(chlyz):
colorscheme base16-bright


