set background=light
highlight clear

if exists("syntax_on")
  syntax reset
endif
let g:colors_name="Modus Operandi"

let s:bg_main                = "#ffffff"
let s:fg_main                = "#000000"
let s:bg_alt                 = "#f0f0f0"
let s:fg_alt                 = "#505050"
let s:bg_active              = "#d7d7d7"
let s:fg_active              = "#0a0a0a"
let s:bg_inactive            = "#efefef"
let s:fg_inactive            = "#404148"
let s:bg_dim                 = "#f8f8f8"
let s:bg_hl_line             = "#f2eff3"
let s:bg_region              = "#bcbcbc"
let s:bg_paren_match         = "#e0af82"
let s:red                    = "#a60000"
let s:green                  = "#005e00"
let s:yellow                 = "#813e00"
let s:blue                   = "#0031a9"
let s:blue_alt               = "#2544bb"
let s:magenta                = "#721045"
let s:magenta_alt_other      = "#5317ac"
let s:magenta_faint          = "#752f50"
let s:cyan                   = "#00538b"
let s:cyan_alt_other         = "#005a5f"
let s:bg_diff_heading        = "#b7cfe0"
let s:fg_diff_heading        = "#041645"
let s:bg_diff_added          = "#d4fad4"
let s:fg_diff_added          = "#004500"
let s:bg_diff_added_deuteran = "#daefff"
let s:fg_diff_added_deuteran = "#002044"
let s:bg_diff_changed        = "#fcefcf"
let s:fg_diff_changed        = "#524200"
let s:bg_diff_removed        = "#ffe8ef"
let s:fg_diff_removed        = "#691616"

" These are not in the proper theme but emacs applies them somewhere.
let s:bg_search              = "#5f9ea0"
let s:bg_incsearch           = "#ee6a50"

function! g:ModusHighlight(group, guibg, guifg, gui)
  exec "highlight " . a:group . " guibg=" . a:guibg . " guifg=" . a:guifg . " gui=" . a:gui
endfunction

function <SID>highlight(group, guibg, guifg, gui)
  call g:ModusHighlight(a:group, a:guibg, a:guifg, a:gui)
endfunction

" Diff highlighting
call <SID>highlight("DiffAdd",     s:bg_diff_added_deuteran, s:fg_diff_added_deuteran, "None")
call <SID>highlight("DiffChange",  s:bg_diff_changed,        s:fg_diff_changed,        "None")
call <SID>highlight("DiffDelete",  s:bg_diff_removed,        s:fg_diff_removed,        "bold")
call <SID>highlight("DiffText",    s:bg_diff_changed,        s:fg_diff_changed,        "bold")
call <SID>highlight("DiffAdded",   s:bg_diff_added_deuteran, s:fg_diff_added_deuteran, "None")
call <SID>highlight("DiffRemoved", s:bg_diff_removed,        s:fg_diff_removed,        "None")
call <SID>highlight("DiffLine",    s:bg_alt,                 s:fg_alt,                 "bold")

" Fugitive highlighting
call <SID>highlight("fugitiveCount",             "None", s:blue,    "None")
call <SID>highlight("fugitiveHash",              "None", s:fg_alt,  "None")
call <SID>highlight("fugitiveHeader",            "None", "None",    "None")
call <SID>highlight("fugitiveHunk",              "None", "None",    "None")
call <SID>highlight("fugitiveUnstagedHeading",   "None", s:cyan,    "bold")
call <SID>highlight("fugitiveUnstagedSection",   "None", "None",    "None")
call <SID>highlight("fugitiveUnstagedModifier",  "None", s:magenta, "bold")
call <SID>highlight("fugitiveUntrackedHeading",  "None", s:cyan,    "bold")
call <SID>highlight("fugitiveUntrackedModifier", "None", s:magenta, "bold")
call <SID>highlight("fugitiveUntrackedSection",  "None", "None",    "None")
call <SID>highlight("fugitiveSymbolicRef",       "None", s:blue,    "None")

" Vim editor colors
call <SID>highlight("Normal",       s:bg_main,        s:fg_main,   "None")
call <SID>highlight("Bold",         "None",           "None",      "bold")
call <SID>highlight("Debug",        "None",           "None",      "None")
call <SID>highlight("Directory",    "None",           s:blue,      "bold")
call <SID>highlight("Error",        "None",           "None",      "None")
call <SID>highlight("ErrorMsg",     "None",           "None",      "None")
call <SID>highlight("Exception",    "None",           "None",      "None")
call <SID>highlight("FoldColumn",   "None",           "None",      "None")
call <SID>highlight("Folded",       "None",           "None",      "None")
call <SID>highlight("IncSearch",    s:bg_incsearch,   s:bg_main,   "bold")
call <SID>highlight("Italic",       "None",           "None",      "italic")
call <SID>highlight("Macro",        "None",           "None",      "None")
call <SID>highlight("MatchParen",   s:bg_paren_match, "None",      "None")
call <SID>highlight("ModeMsg",      "None",           "None",      "None")
call <SID>highlight("MoreMsg",      "None",           "None",      "None")
call <SID>highlight("Question",     "None",           "None",      "None")
call <SID>highlight("Search",       s:bg_search,      s:bg_main,   "None")
call <SID>highlight("Substitute",   "None",           "None",      "None")
call <SID>highlight("SpecialKey",   "None",           "None",      "None")
call <SID>highlight("TooLong",      "None",           "None",      "None")
call <SID>highlight("Underlined",   "None",           "None",      "None")
call <SID>highlight("Visual",       s:bg_region,      s:fg_main,   "None")
call <SID>highlight("VisualNOS",    "None",           "None",      "None")
call <SID>highlight("WarningMsg",   "None",           "None",      "None")
call <SID>highlight("WildMenu",     "None",           "None",      "None")
call <SID>highlight("Title",        "None",           "None",      "None")
call <SID>highlight("Conceal",      "None",           "None",      "None")
call <SID>highlight("Cursor",       "None",           "None",      "None")
call <SID>highlight("NonText",      "None",           "None",      "None")
call <SID>highlight("LineNr",       s:bg_dim,         "None",      "None")
call <SID>highlight("SignColumn",   "None",           "None",      "None")
call <SID>highlight("StatusLine",   s:bg_active,      "None",      "None")
call <SID>highlight("StatusLineNC", s:bg_inactive,    "None",      "None")
call <SID>highlight("VertSplit",    "None",           "None",      "None")
call <SID>highlight("ColorColumn",  "None",           "None",      "None")
call <SID>highlight("CursorColumn", "None",           "None",      "None")
call <SID>highlight("CursorLine",   s:bg_hl_line,     "None",      "None")
call <SID>highlight("CursorLineNr", s:bg_active,      "None",      "bold")
call <SID>highlight("QuickFixLine", "None",           "None",      "None")
call <SID>highlight("PMenu",        "None",           "None",      "None")
call <SID>highlight("PMenuSel",     s:bg_hl_line,     s:blue,      "bold")
call <SID>highlight("TabLine",      "None",           "None",      "None")
call <SID>highlight("TabLineFill",  "None",           "None",      "None")
call <SID>highlight("TabLineSel",   "None",           "None",      "None")

" Standard syntax highlighting
call <SID>highlight("Boolean",      "None",  "None",               "None")
call <SID>highlight("Character",    "None",  "None",               "None")
call <SID>highlight("Comment",      "None",  s:fg_alt,             "None")
call <SID>highlight("Conditional",  "None",  s:magenta_alt_other,  "None")
call <SID>highlight("Constant",     "None",  "None",               "None")
call <SID>highlight("Define",       "None",  "None",               "None")
call <SID>highlight("Delimiter",    "None",  "None",               "None")
call <SID>highlight("Float", "       None",  "None",               "None")
call <SID>highlight("Function",     "None",  s:magenta_alt_other,  "None")
call <SID>highlight("Identifier",   "None",  "None",               "None")
call <SID>highlight("Include",      "None",  s:red,                "None")
call <SID>highlight("Keyword",      "None",  "None",               "None")
call <SID>highlight("Label",        "None",  s:magenta_alt_other,  "None")
call <SID>highlight("Number",       "None",  "None",               "None")
call <SID>highlight("Operator",     "None",  s:magenta,            "None")
call <SID>highlight("PreProc",      "None",  "None",               "None")
call <SID>highlight("Repeat",       "None",  s:magenta_alt_other,  "None")
call <SID>highlight("Special",      "None",  "None",               "None")
call <SID>highlight("SpecialChar",  "None",  s:magenta,            "None")
call <SID>highlight("Statement",    "None",  "None",               "None")
call <SID>highlight("StorageClass", "None",  s:magenta_alt_other,  "None")
call <SID>highlight("String",       "None",  s:blue_alt,           "None")
call <SID>highlight("Structure",    "None",  "None",               "None")
call <SID>highlight("Tag",          "None",  "None",               "None")
call <SID>highlight("Todo",         "None",  "None",               "None")
call <SID>highlight("Type",         "None",  s:cyan,               "None")
call <SID>highlight("Typedef",      "None",  "None",               "None")

" Tree sitter highlighting
call <SID>highlight("TSConstant",        "None", s:blue,              "None")
call <SID>highlight("TSFunction",        "None", s:magenta_faint,     "None")
call <SID>highlight("TSFuncMacro",       "None", s:red,               "None")
call <SID>highlight("TSInclude",         "None", s:red,               "None")
call <SID>highlight("TSKeyword",         "None", s:magenta_alt_other, "None")
call <SID>highlight("TSKeywordFunction", "None", s:magenta_alt_other, "None")
call <SID>highlight("TSNote",            "None", s:cyan,              "None")
call <SID>highlight("TSNumber",          "None", s:blue,              "None")
call <SID>highlight("TSWarning",         "None", s:yellow,            "None")
call <SID>highlight("TSType",            "None", s:cyan_alt_other,    "None")
call <SID>highlight("TSVariable",        "None", s:cyan,              "None")
call <SID>highlight("TSVariableBuiltin", "None", s:cyan,              "None")

" C highlighting
call <SID>highlight("cOperator",  "None", "None", "None")
call <SID>highlight("cPreCondit", "None", s:red,  "None")
call <SID>highlight("cType",      "None", s:cyan, "None")

" Netrw highlight
highlight link netrwDir Directory

" Remove variables
unlet s:bg_main s:fg_main s:bg_dim  s:bg_alt s:fg_alt s:bg_hl_line s:bg_active s:fg_active
unlet s:bg_inactive s:fg_inactive s:bg_region s:bg_paren_match s:bg_search s:bg_incsearch
unlet s:red s:cyan s:cyan_alt_other s:green s:blue s:blue_alt s:yellow s:magenta s:magenta_alt_other s:magenta_faint
unlet s:bg_diff_heading s:fg_diff_heading s:bg_diff_added s:fg_diff_added s:bg_diff_added_deuteran s:fg_diff_added_deuteran
unlet s:bg_diff_changed s:fg_diff_changed s:bg_diff_removed s:fg_diff_removed

" Remove functions
delf <SID>highlight
