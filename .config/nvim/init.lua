require("impatient")
require("lyzell.init")

-- TODO: Rewrite in lua.
vim.api.nvim_exec(
[[
function CheckColors()
  if filereadable(expand("~/.local/share/.background"))
    for line in readfile(expand("~/.local/share/.background"), '', 1)
      if line =~ "light"
        colorscheme modus-operandi
      else
        colorscheme modus-vivendi
      endif
    endfor
  endif
endfunction

function ToggleColors()
    call system('theme-toggle')
    call system('source "$HOME/.bashrc"')
    call CheckColors()
endfunction

call CheckColors()

autocmd FocusGained * call CheckColors()

" Put these in an autocmd group, so that you can revert them with:
" ":augroup vimStartup | exe 'au!' | augroup END"
augroup vimStartup
  au!

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid, when inside an event handler
  " (happens when dropping a file on gvim) and for a commit message (it's
  " likely a different one than last time).
  autocmd BufReadPost *
  \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
  \ |   exe "normal! g`\""
  \ | endif

augroup END

augroup highlight_yank
    autocmd!
    au TextYankPost * silent! lua vim.highlight.on_yank{higroup="IncSearch", timeout=100}
augroup END

let g:gutentags_ctags_exclude = [
      \ '*bazel-*', '.git', 'Build', '.ccls-cache', 'MDF4', 'HDF5',
      \ 'Compiler', 'sqlite3.*', 'Rte_*.*', 'VehicleModel', 'ProtoBuf',
      \ 'Protobuf', '*.cs'
            \]

let g:CommandTPreferredImplementation='lua'
]],
true)

vim.cmd("abbrev td TODO:")


-- You dont need to set any of these options. These are the default ones. Only
-- the loading is important
require('telescope').setup {
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    }
  }
}
-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require('telescope').load_extension('fzf')
