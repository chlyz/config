require("impatient")
require("lyzell.init")

-- TODO: Rewrite in lua.
vim.api.nvim_exec(
[[

set diffopt+=iwhite
set diffexpr=DiffW()

function DiffW()
    let opt = ""
    if &diffopt =~ "icase"
        let opt = opt . "-i "
    endif
    if &diffopt =~ "iwhite"
        let opt = opt . "-w "
    endif
    silent execute "!diff -a --binary " . opt .
        \ v:fname_in . " " . v:fname_new .  " > " . v:fname_out
endfunction

let g:surround_no_insert_mappings = 1

let g:modus_vivendi_italic_constructs = 1

function CheckColors()
  if filereadable(expand("~/.local/share/.background"))
    for line in readfile(expand("~/.local/share/.background"), '', 1)
      if line =~ "light"
        colorscheme modus-operandi
      else
        colorscheme tokyo-vivendi
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

let g:LoupeCenterResults=0
" autocmd FocusGained * call CheckColors()

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
    au TextYankPost * silent! lua vim.highlight.on_yank{higroup="TextYankPost", timeout=200}
augroup END

autocmd BufRead,BufNewFile Jenkinsfile set filetype=groovy

let g:gutentags_ctags_exclude = [
      \ '*bazel-*', '.git', 'Build', '.ccls-cache', 'MDF4', 'HDF5',
      \ 'Compiler', 'sqlite3.*', 'Rte_*.*', 'VehicleModel', 'ProtoBuf',
      \ 'Protobuf', '*.cs'
            \]

]],
true)

vim.cmd("abbrev td TODO:")
vim.cmd("abbrev tm TODO(chlyz):")
