vim.o.number = true
vim.o.relativenumber = true
vim.o.wrap = false
vim.o.mouse='a'
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.expandtab = true
vim.o.shiftwidth=4
vim.o.softtabstop=4
vim.o.smartindent = true
vim.o.termguicolors = true
vim.o.cursorline = true
-- set guicursor=n-c-v:blinkwait300-blinkon200-blinkoff150,i-ci-ve:blinkon1-ver25,r-cr-o:hor20-blinkon1
-- set wildignore+=*.o,*.rej,*.so
vim.o.wildignorecase = true
vim.o.wildmode='longest:full,full'
vim.o.emoji = false
vim.opt.completeopt = { "menu", "menuone", "noselect" }
vim.opt.shortmess:append "c"
vim.opt.inccommand = 'split'
vim.opt.textwidth = 100
-- vim.opt.hidden = false

-- vim.opt.linebreak = true                              -- wrap long lines at characters in 'breakat'

vim.opt.showbreak = '↳ '
vim.opt.fillchars = {
  diff            = '∙',                              -- BULLET OPERATOR (U+2219, UTF-8: E2 88 99)
  -- eob             = ' ',                              -- NO-BREAK SPACE (U+00A0, UTF-8: C2 A0) to suppress ~ at EndOfBuffer
  eob             = ' ',                              -- NO-BREAK SPACE (U+00A0, UTF-8: C2 A0) to suppress ~ at EndOfBuffer
  fold            = '·',                              -- MIDDLE DOT (U+00B7, UTF-8: C2 B7)
  vert            = '┃',                              -- BOX DRAWINGS HEAVY VERTICAL (U+2503, UTF-8: E2 94 83)
}

vim.opt.list           = false
vim.opt.listchars      = {
  eol                  = '↲',
  nbsp                 = '␣',
  extends              = '»',
  precedes             = '«',
  tab                  = '» ',
  trail                = '•',
}

vim.opt.shell = 'sh'

vim.opt.completeopt = 'menu'
vim.opt.completeopt = vim.opt.completeopt + 'menuone'
vim.opt.completeopt = vim.opt.completeopt + 'noselect'
