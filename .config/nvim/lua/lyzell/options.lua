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
vim.o.wildmode='longest:full,full'
vim.o.emoji = false
vim.opt.completeopt = { "menu", "menuone", "noselect" }
vim.opt.shortmess:append "c"

-- vim.opt.linebreak = true                              -- wrap long lines at characters in 'breakat'

vim.opt.showbreak = '↳ '
vim.opt.fillchars = {
  diff            = '∙',                              -- BULLET OPERATOR (U+2219, UTF-8: E2 88 99)
  -- eob             = ' ',                              -- NO-BREAK SPACE (U+00A0, UTF-8: C2 A0) to suppress ~ at EndOfBuffer
  eob             = ' ',                              -- NO-BREAK SPACE (U+00A0, UTF-8: C2 A0) to suppress ~ at EndOfBuffer
  fold            = '·',                              -- MIDDLE DOT (U+00B7, UTF-8: C2 B7)
  vert            = '┃',                              -- BOX DRAWINGS HEAVY VERTICAL (U+2503, UTF-8: E2 94 83)
}

vim.opt.list           = true                              -- show whitespace
vim.opt.listchars      = {
  nbsp                 = '⦸',                              -- CIRCLED REVERSE SOLIDUS (U+29B8, UTF-8: E2 A6 B8)
  extends              = '»',                              -- RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK (U+00BB, UTF-8: C2 BB)
  precedes             = '«',                              -- LEFT-POINTING DOUBLE ANGLE QUOTATION MARK (U+00AB, UTF-8: C2 AB)
  tab                  = '▷⋯',                             -- WHITE RIGHT-POINTING TRIANGLE (U+25B7, UTF-8: E2 96 B7) + MIDLINE HORIZONTAL ELLIPSIS (U+22EF, UTF-8: E2 8B AF)
  trail                = '•',                              -- BULLET (U+2022, UTF-8: E2 80 A2)
}

vim.opt.shell = 'sh'

vim.opt.completeopt = 'menu'
vim.opt.completeopt = vim.opt.completeopt + 'menuone'
vim.opt.completeopt = vim.opt.completeopt + 'noselect'
