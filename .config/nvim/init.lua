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

let g:CommandTPreferredImplementation='lua'
let g:fzf_preview_window = []
]],
true)

vim.cmd("abbrev td TODO:")
vim.cmd("abbrev tm TODO(chlyz):")

-- You dont need to set any of these options. These are the default ones. Only
-- the loading is important
-- require('telescope').setup {
--   extensions = {
--     fzf = {
--       fuzzy = true,                    -- false will only do exact matching
--       override_generic_sorter = true,  -- override the generic sorter
--       override_file_sorter = true,     -- override the file sorter
--       case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
--                                        -- the default case_mode is "smart_case"
--     }
--   }
-- }
-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
-- require('telescope').load_extension('fzf')

-- require("tokyonight").setup({
--     style = "night",
-- })

function vivendi_colors()
    return {
        bg_main = "#000000",
        fg_main = "#ffffff",
        bg_dim = "#100f10",
        fg_dim = "#e0e6f0",
        bg_alt = "#191a1b",
        fg_alt = "#a8a8a8",
        -- specifically for on/off states and must be combined with themselves, though the
        -- backgrounds are also meant to be used with other "active" values, defined further below;
        -- bg_active_accent can work as a substitute for bg_active
        bg_active = "#323232",
        fg_active = "#f4f4f4",
        bg_inactive = "#1e1e1e",
        fg_inactive = "#bfc0c4",
        bg_active_accent = "#2a2a66",
        -- these special values are intended as alternatives to the base values for cases where we
        -- need to avoid confusion between the highlighted constructs; they must either be used as
        -- pairs based on their name or each can be combined with {fg,bg}_{main,alt,dim} always in
        -- accordance with their role as background or foreground
        bg_special_cold = "#203448",
        bg_special_faint_cold = "#0e183a",
        fg_special_cold = "#c6eaff",
        bg_special_mild = "#00322e",
        bg_special_faint_mild = "#001f1a",
        fg_special_mild = "#bfebe0",
        bg_special_warm = "#382f27",
        bg_special_faint_warm = "#241613",
        fg_special_warm = "#f8dec0",
        bg_special_calm = "#392a48",
        bg_special_faint_calm = "#251232",
        fg_special_calm = "#fbd6f4",
        -- foregrounds that can be combined with bg_main, bg_dim, bg_alt
        red = "#ff8059",
        red_alt = "#ef8b50",
        red_alt_other = "#ff9077",
        red_faint = "#ffa0a0",
        red_alt_faint = "#f5aa80",
        red_alt_other_faint = "#ff9fbf",
        green = "#44bc44",
        green_alt = "#70b900",
        green_alt_other = "#00c06f",
        green_faint = "#78bf78",
        green_alt_faint = "#99b56f",
        green_alt_other_faint = "#88bf99",
        yellow = "#d0bc00",
        yellow_alt = "#c0c530",
        yellow_alt_other = "#d3b55f",
        yellow_faint = "#d2b580",
        yellow_alt_faint = "#cabf77",
        yellow_alt_other_faint = "#d0ba95",
        blue = "#2fafff",
        blue_alt = "#79a8ff" ,
        blue_alt_other = "#00bcff",
        blue_faint = "#82b0ec",
        blue_alt_faint = "#a0acef",
        blue_alt_other_faint = "#80b2f0",
        magenta = "#feacd0",
        magenta_alt = "#f78fe7",
        magenta_alt_other = "#b6a0ff",
        magenta_faint = "#e0b2d6",
        magenta_alt_faint = "#ef9fe4",
        magenta_alt_other_faint = "#cfa6ff",
        cyan = "#00d3d0",
        cyan_alt = "#4ae2f0",
        cyan_alt_other = "#6ae4b9",
        cyan_faint = "#90c4ed",
        cyan_alt_faint = "#a0bfdf",
        cyan_alt_other_faint = "#a4d0bb",
        -- these foreground values can only be combined with bg_main and are thus not suitable for
        -- general purpose highlighting
        red_intense = "#fe6060",
        orange_intense = "#fba849",
        green_intense = "#4fe42f",
        yellow_intense = "#f0dd60",
        blue_intense = "#4fafff",
        magenta_intense = "#ff62d4",
        purple_intense = "#9f80ff",
        cyan_intense = "#3fdfd0",
        -- those foregrounds are meant exclusively for bg_active, bg_inactive
        red_active = "#ffa7ba",
        green_active = "#70d73f",
        yellow_active = "#dbbe5f",
        blue_active = "#34cfff",
        magenta_active = "#d5b1ff",
        cyan_active = "#00d8b4",
        -- the "subtle" values below be combined with fg_dim, while the "intense" should be paired
        -- with fg_main
        red_subtle_bg = "#762422",
        red_intense_bg = "#a4202a",
        green_subtle_bg = "#2f4a00",
        green_intense_bg = "#006800",
        yellow_subtle_bg = "#604200",
        yellow_intense_bg = "#874900",
        blue_subtle_bg = "#10387c",
        blue_intense_bg = "#2a40b8",
        magenta_subtle_bg = "#49366e",
        magenta_intense_bg = "#7042a2",
        cyan_subtle_bg = "#00415e",
        cyan_intense_bg = "#005f88",
        -- those background values must be combined with fg_main and should only be used for
        -- indicators that are placed on the fringes
        red_fringe_bg = "#8f1f4b",
        green_fringe_bg = "#006700",
        yellow_fringe_bg = "#6f4f00",
        blue_fringe_bg = "#3f33af",
        magenta_fringe_bg = "#6f2f89",
        cyan_fringe_bg = "#004f8f",
        -- those background values should only be used for graphs or similar applications where
        -- colored blocks are expected to be positioned next to each other
        red_graph_0_bg = "#b52c2c",
        red_graph_1_bg = "#702020",
        green_graph_0_bg = "#4fd100",
        green_graph_1_bg = "#007800",
        yellow_graph_0_bg = "#f1e00a",
        yellow_graph_1_bg = "#b08600",
        blue_graph_0_bg = "#2fafef",
        blue_graph_1_bg = "#1f2f8f",
        magenta_graph_0_bg = "#bf94fe",
        magenta_graph_1_bg = "#5f509f",
        cyan_graph_0_bg = "#47dfea",
        cyan_graph_1_bg = "#00808f",
        -- the following are for cases where both the foreground and the background need to have a
        -- similar hue and so must be combined with themselves, even though the foregrounds can be
        -- paired with any of the base backgrounds
        red_refine_bg = "#77002a",
        red_refine_fg = "#ffb9ab",
        green_refine_bg = "#00422a",
        green_refine_fg = "#9ff0cf",
        yellow_refine_bg = "#693200",
        yellow_refine_fg = "#e2d980",
        blue_refine_bg = "#242679",
        blue_refine_fg = "#8ecfff",
        magenta_refine_bg = "#71206a",
        magenta_refine_fg = "#ffcaf0",
        cyan_refine_bg = "#004065",
        cyan_refine_fg = "#8ae4f2",
        -- the "nuanced" backgrounds can be combined with all of the above foregrounds, as well as
        -- those included here, while the "nuanced" foregrounds can in turn also be combined with
        -- bg_main, bg_dim, bg_alt
        red_nuanced_bg = "#2c0614",
        red_nuanced_fg = "#ffcccc",
        green_nuanced_bg = "#001904",
        green_nuanced_fg = "#b8e2b8",
        yellow_nuanced_bg = "#221000",
        yellow_nuanced_fg = "#dfdfb0",
        blue_nuanced_bg = "#0f0e39",
        blue_nuanced_fg = "#bfd9ff",
        magenta_nuanced_bg = "#230631",
        magenta_nuanced_fg = "#e5cfef",
        cyan_nuanced_bg = "#041529",
        cyan_nuanced_fg = "#a8e5e5",
        -- the following are reserved for specific cases
        --
        -- bg_hl_line is between bg_dim and bg_alt, so it should work with all accents that cover
        -- those two, plus bg_main
        --
        -- bg_hl_alt and bg_hl_alt_intense should only be used when no other grayscale or fairly
        -- neutral background is available to properly draw attention to a given construct
        --
        -- bg_header is between bg_active and bg_inactive, so it can be combined with any of the
        -- "active" values, plus the "special" and base foreground colors
        --
        -- bg_paren_match, bg_paren_match_intense, bg_region, bg_region_accent and bg_tab_active
        -- must be combined with fg_main, while bg_tab_inactive should be combined with fg_dim,
        -- whereas bg_tab_inactive_alt goes together with fg_main
        --
        -- bg_completion_* and bg_char_* variants are meant to be combined with fg_main
        --
        --
        -- fg_lang_error, fg_lang_warning, fg_lang_note can be combined with bg_main, bg_dim, bg_alt
        --
        -- fg_mark_sel, fg_mark_del, fg_mark_alt can be combined with bg_main, bg_dim, bg_alt,
        -- bg_hl_line
        --
        --
        --
        -- all pairs are combinable with themselves
        bg_hl_line = "#151823",
        bg_inactive_hl_line_intense = "#292929",
        bg_hl_line_intense_accent = "#002a4f",
        bg_hl_alt = "#181732",
        bg_hl_alt_intense = "#282e46",
        bg_paren_match = "#6f3355",
        bg_paren_match_intense = "#7416b5",
        bg_paren_expression = "#221044",
        bg_region = "#3c3c3c",
        bg_region_accent = "#4f3d88",
        bg_region_accent_subtle = "#240f55",

        bg_completion = "#142f69",
        bg_completion_subtle = "#0e194b",

        bg_char_0 = "#0050af",
        bg_char_1 = "#7f1f7f",
        bg_char_2 = "#625a00",

        bg_tab_active = "#0e0e0e",
        bg_tab_inactive = "#424242",
        bg_tab_inactive_accent = "#35398f",
        bg_tab_inactive_alt = "#595959",
        bg_tab_inactive_alt_accent = "#505588",

        red_tab = "#ffc0bf",
        green_tab = "#88ef88",
        yellow_tab = "#d2e580",
        orange_tab = "#f5ca80",
        blue_tab = "#92d9ff",
        cyan_tab = "#60e7e0",
        magenta_tab = "#ffb8ff",
        purple_tab = "#cfcaff",

        -- fg_escape_char_construct and fg_escape_char_backslash can be combined bg_main, bg_dim,
        -- and bg_alt
        fg_escape_char_construct = "#e7a59a",
        fg_escape_char_backslash = "#abab00",

        fg_lang_error = "#ef8690",
        fg_lang_warning = "#b0aa00",
        fg_lang_note = "#9d9def",
        fg_lang_underline_error = "#ff4a6f",
        fg_lang_underline_warning = "#d0de00",
        fg_lang_underline_note = "#5f6fff",

        -- the window divider colors apply to faces with just an fg value
        fg_window_divider_inner = "#646464",
        fg_window_divider_outer = "#969696",

        -- fg_unfocused must be combined with bg_main
        fg_unfocused = "#93959b",

        -- fg_docstring, fg_comment_yellow can be combined with bg_main, bg_dim, and bg_alt
        fg_docstring = "#b0d6f5",
        fg_comment_yellow = "#d0a070",

        bg_header = "#212121",
        fg_header = "#dddddd",

        bg_whitespace = "#101424",
        fg_whitespace = "#aa9e9f",

        bg_diff_heading = "#304466",
        fg_diff_heading = "#dae7ff",
        bg_diff_added = "#0a280a",
        fg_diff_added = "#94ba94",
        bg_diff_added_deuteran = "#001a3f",
        fg_diff_added_deuteran = "#c4cdf2",
        bg_diff_changed = "#2a2000",
        fg_diff_changed = "#b0ba9f",
        bg_diff_removed = "#40160f",
        fg_diff_removed = "#c6adaa",

        bg_diff_refine_added = "#005a36",
        fg_diff_refine_added = "#e0f6e0",
        bg_diff_refine_added_deuteran = "#234f8f",
        fg_diff_refine_added_deuteran = "#dde4ff",
        bg_diff_refine_changed = "#585800",
        fg_diff_refine_changed = "#ffffcc",
        bg_diff_refine_removed = "#852828",
        fg_diff_refine_removed = "#ffd9eb",

        bg_diff_focus_added = "#1d3c25",
        fg_diff_focus_added = "#b4ddb4",
        bg_diff_focus_added_deuteran = "#003959",
        fg_diff_focus_added_deuteran = "#bfe4ff",
        bg_diff_focus_changed = "#424200",
        fg_diff_focus_changed = "#d0daaf",
        bg_diff_focus_removed = "#601f29",
        fg_diff_focus_removed = "#eebdba",

        bg_mark_sel = "#002f2f",
        fg_mark_sel = "#60cfa2",
        bg_mark_del = "#5a0000",
        fg_mark_del = "#ff99aa",
        bg_mark_alt = "#3f2210",
        fg_mark_alt = "#f0aa20",
    }
end

-- require("tokyonight").setup({
--     style = "night",
-- })

require("tokyonight").setup({
    style = "night",
    on_colors = function(colors)
        local modus = vivendi_colors()
        colors.bg               = modus.bg_main
        colors.bg_highlight     = modus.bg_hl_line
        colors.bg_float         = modus.bg_dim
        colors.bg_sidebar       = modus.bg_dim
        colors.fg               = modus.fg_main
        colors.fg_gutter        = modus.fg_alt
        colors.fg_dark          = modus.fg_dim
        colors.fg_sidebar       = modus.fg_dim
        colors.fg               = modus.fg_main
        colors.orange           = modus.orange_intense
        colors.comment          = modus.fg_alt
        colors.green            = modus.green_alt
        colors.green1           = modus.cyan_alt_other
        colors.magenta          = modus.magenta_alt
        colors.purple           = modus.magenta_alt_other
        colors.cyan             = modus.blue_alt_other
        colors.blue             = modus.blue_alt
        colors.blue1            = modus.cyan_alt
        colors.blue5            = modus.blue_alt_other
        colors.error            = modus.fg_lang_error
        colors.info             = modus.fg_lang_note
        colors.border           = modus.fg_window_divider_inner
        colors.border_highlight = modus.fg_window_divider_outer
    end,
    on_highlights = function(hl, c)
        local modus = vivendi_colors()
        hl.CursorLineNr = { bg = modus.bg_active, bold = true }
        hl.DiffAdd = {bg = modus.bg_diff_focus_added, fg = modus.fg_diff_focus_added }
        hl.DiffChange = {bg = modus.bg_diff_changed, fg = modus.fg_diff_changed }
        hl.DiffDelete = {bg = modus.bg_diff_focus_removed, fg = modus.fg_diff_focus_removed, bold = true }
        -- hl.DiffLine = {bg = modus.bg_alt, fg = modus.fg_alt, bold = true }
        hl.DiffText = {bg = modus.bg_diff_focus_changed, fg = modus.fg_diff_focus_changed, bold = true }
        hl.Folded = {bg = modus.bg_alt, fg = modus.fg_alt, italic = true }
        hl.IncSearch = { bg = modus.yellow_intense_bg, fg = modus.fg_dim }
        hl.LineNr = { bg = modus.bg_alt, fg = c.fg_gutter }
        hl.MatchParen = { bg = modus.bg_paren_match }
        hl.NormalFloat = { fg = modus.fg_dim, bg = c.bg_float }
        hl.Pmenu = { bg = modus.bg_alt }
        hl.PmenuSel = { bg = modus.bg_active, fg = modus.fg_active, bold = true }
        hl.Search = { bg = modus.cyan_subtle_bg, fg = c.fg }
        hl.StatusLine = { bg = modus.bg_active, fg = modus.fg_active }
        hl.StatusLineNC = { bg = modus.bg_inactive, fg = modus.fg_inactive }
        hl.Substitute = { bg = modus.yellow_refine_bg, fg = modus.yellow_refine_fg }
        hl.TSDanger = { fg = c.error }
        hl.TSNote = { fg = c.info }
        hl.TSWarning = { fg = c.warning }
        hl.TSVariableBuiltin = { fg = modus.magenta_alt }
        hl.TextYankPost = { bg = modus.bg_active_accent }
        hl.Todo = { fg = modus.fg_lang_warning }
        hl.Visual = { bg = modus.bg_region, fg = c.fg }
        hl.VisualNOS = { bg = modus.bg_region, fg = c.fg }
        hl.diffAdded = {bg = modus.bg_diff_focus_added, fg = modus.fg_diff_focus_added }
        hl.diffChanged = {bg = modus.bg_diff_focus_changed, fg = modus.fg_diff_focus_changed }
        hl.diffRemoved = {bg = modus.bg_diff_focus_removed, fg = modus.fg_diff_removed, bold = true }
        hl.QuickFixLine = { bg = modus.bg_hl_alt, bold = true }
        hl.qfLineNr = { fg = modus.fg_special_warm, underline = true }
        hl.qfFileName = { fg = modus.fg_special_cold, underline = true }
    end,
})

vim.cmd("colorscheme tokyonight")
