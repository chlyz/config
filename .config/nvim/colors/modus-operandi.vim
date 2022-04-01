set background=light
highlight clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name="Modus Operandi"

let s:bg_main                = "#ffffff"
let s:bg_dim                 = "#f8f8f8"
let s:bg_alt                 = "#f0f0f0"
let s:bg_hl_line             = "#f2eff3"
let s:bg_region              = "#bcbcbc"
let s:bg_special_mild        = "#c4ede0"
let s:bg_paren_match         = "#e0af82"
let s:bg_paren_match_intense = "#c488ff"

let s:fg_main                = "#000000"
let s:fg_dim                 = "#282828"
let s:fg_alt                 = "#505050"

let s:bg_active        = "#d7d7d7"
let s:fg_active        = "#0a0a0a"
let s:bg_inactive      = "#efefef"
let s:fg_inactive      = "#404148"
let s:bg_active_accent = "#d0d6ff"

let s:red                    = "#a60000"
let s:red_alt                = "#972500"
let s:red_alt_other          = "#a0132f"
let s:red_faint              = "#7f1010"
let s:red_alt_faint          = "#702f00"
let s:red_alt_other_faint    = "#7f002f"
let s:red_subtle_bg          = "#f2b0a2"
let s:red_intense_bg         = "#ff9f9f"

let s:green                  = "#005e00"
let s:green_alt              = "#315b00"
let s:green_alt_other        = "#145c33"
let s:green_faint            = "#104410"
let s:green_alt_faint        = "#30440f"
let s:green_alt_other_faint  = "#0f443f"
let s:green_subtle_bg        = "#aecf90"
let s:green_intense_bg       = "#5ada88"
let s:green_refine_bg        = "#aceaac"

let s:yellow                 = "#813e00"
let s:yellow_alt             = "#70480f"
let s:yellow_alt_other       = "#863927"
let s:yellow_faint           = "#5f4400"
let s:yellow_alt_faint       = "#5d5000"
let s:yellow_alt_other_faint = "#5e3a20"
let s:yellow_subtle_bg       = "#e4c340"
let s:yellow_intense_bg      = "#f5df23"

let s:blue                   = "#0031a9"
let s:blue_alt               = "#2544bb"
let s:blue_alt_other         = "#0000c0"
let s:blue_faint             = "#003497"
let s:blue_alt_faint         = "#0f3d8c"
let s:blue_alt_other_faint   = "#001087"
let s:blue_subtle_bg         = "#b5d0ff"
let s:blue_intense_bg        = "#77baff"

let s:magenta                = "#721045"
let s:magenta_alt            = "#8f0075"
let s:magenta_alt_other      = "#5317ac"
let s:magenta_faint          = "#752f50"
let s:magenta_alt_faint      = "#7b206f"
let s:magenta_alt_other_faint= "#55348e"

let s:cyan                   = "#00538b"
let s:cyan_alt               = "#30517f"
let s:cyan_alt_other         = "#005a5f"
let s:cyan_faint             = "#005077"
let s:cyan_alt_faint         = "#354f6f"
let s:cyan_alt_other_faint   = "#125458"
let s:cyan_active            = "#003f8a"
let s:cyan_subtle_bg         = "#c0efff"
let s:cyan_intense_bg        = "#42cbd4"

let s:red_intense            = "#fe6060"
let s:orange_intense         = "#fba849"
let s:green_intense          = "#4fe42f"
let s:yellow_intense         = "#f0dd60"
let s:blue_intense           = "#4fafff"
let s:magenta_intense        = "#ff62d4"
let s:purple_intense         = "#9f80ff"
let s:cyan_intense           = "#3fdfd0"

" These are not in the proper theme but emacs applies them somewhere.
let s:bg_search    = "#5f9ea0"
let s:bg_incsearch = "#ee6a50"

let s:bg_diff_heading               = "#b7cfe0"
let s:fg_diff_heading               = "#041645"
let s:bg_diff_added                 = "#d4fad4"
let s:fg_diff_added                 = "#004500"
let s:bg_diff_added_deuteran        = "#daefff"
let s:fg_diff_added_deuteran        = "#002044"
let s:bg_diff_changed               = "#fcefcf"
let s:fg_diff_changed               = "#524200"
let s:bg_diff_removed               = "#ffe8ef"
let s:fg_diff_removed               = "#691616"
let s:bg_diff_refine_added          = "#94cf94"
let s:fg_diff_refine_added          = "#002a00"
let s:bg_diff_refine_added_deuteran = "#77c0ef"
let s:fg_diff_refine_added_deuteran = "#000035"
let s:bg_diff_refine_changed        = "#cccf8f"
let s:fg_diff_refine_changed        = "#302010"
let s:bg_diff_refine_removed        = "#daa2b0"
let s:fg_diff_refine_removed        = "#400000"
let s:bg_diff_focus_added           = "#bbeabb"
let s:fg_diff_focus_added           = "#002c00"
let s:bg_diff_focus_added_deuteran  = "#bacfff"
let s:fg_diff_focus_added_deuteran  = "#001755"
let s:bg_diff_focus_changed         = "#ecdfbf"
let s:fg_diff_focus_changed         = "#392900"
let s:bg_diff_focus_removed         = "#efcbcf"
let s:fg_diff_focus_removed         = "#4a0000"

" Modus theme highlighting function
function! g:ModusHighlight(group, guibg, guifg, gui)
    exec "highlight " . a:group . " guibg=" . a:guibg . " guifg=" . a:guifg . " gui=" . a:gui
endfunction

function <sid>highlight(group, guibg, guifg, gui)
  call g:ModusHighlight(a:group, a:guibg, a:guifg, a:gui)
endfunction

" Diff highlighting
call <sid>highlight("DiffAdd",     s:bg_diff_added_deuteran, s:fg_diff_added_deuteran, "None")
call <sid>highlight("DiffChange",  s:bg_diff_changed,        s:fg_diff_changed,        "None")
call <sid>highlight("DiffDelete",  s:bg_diff_removed,        s:fg_diff_removed,        "bold")
call <sid>highlight("DiffText",    s:bg_diff_changed,        s:fg_diff_changed,        "bold")
call <sid>highlight("DiffAdded",   s:bg_diff_added_deuteran, s:fg_diff_added_deuteran, "None")
call <sid>highlight("DiffRemoved", s:bg_diff_removed,        s:fg_diff_removed,        "None")
call <sid>highlight("DiffLine",    s:bg_alt,                 s:fg_alt,                 "bold")
" call <sid>highlight("DiffFile",    s:bg_diff_heading, s:fg_diff_heading, "None")
" call <sid>highlight("DiffNewFile", s:red, "None", "None")

" Fugitive highlighting
call <sid>highlight("fugitiveCount",             "None", s:blue,    "None")
call <sid>highlight("fugitiveHash",              "None", s:fg_alt,  "None")
call <sid>highlight("fugitiveHeader",            "None", "None",    "None")
call <sid>highlight("fugitiveHunk",              "None", "None",    "None")
call <sid>highlight("fugitiveUnstagedHeading",   "None", s:cyan,    "bold")
call <sid>highlight("fugitiveUnstagedSection",   "None", "None",    "None")
call <sid>highlight("fugitiveUnstagedModifier",  "None", s:magenta, "bold")
call <sid>highlight("fugitiveUntrackedHeading",  "None", s:cyan,    "bold")
call <sid>highlight("fugitiveUntrackedModifier", "None", s:magenta, "bold")
call <sid>highlight("fugitiveUntrackedSection",  "None", "None",    "None")
call <sid>highlight("fugitiveSymbolicRef",       "None", s:blue,    "None")

" Vim editor colors
call <sid>highlight("Normal",       s:bg_main,            s:fg_main,            "None")
call <sid>highlight("Bold",         "None",               "None",               "bold")
call <sid>highlight("Debug",        "None",               "None",               "None")
call <sid>highlight("Directory",    "None",               s:blue,               "bold")
call <sid>highlight("Error",        "None",               "None",               "None")
call <sid>highlight("ErrorMsg",     "None",               "None",               "None")
call <sid>highlight("Exception",    "None",               "None",               "None")
call <sid>highlight("FoldColumn",   "None",               "None",               "None")
call <sid>highlight("Folded",       "None",               "None",               "None")
call <sid>highlight("IncSearch",    s:bg_incsearch,       s:bg_main,            "bold")
call <sid>highlight("Italic",       "None",               "None",               "italic")
call <sid>highlight("Macro",        "None",               "None",               "None")
call <sid>highlight("MatchParen",   s:bg_paren_match,     "None",               "None")
call <sid>highlight("ModeMsg",      "None",               "None",               "None")
call <sid>highlight("MoreMsg",      "None",               "None",               "None")
call <sid>highlight("Question",     "None",               "None",               "None")
call <sid>highlight("Search",       s:bg_search,          s:bg_main,            "None")
call <sid>highlight("Substitute",   "None",               "None",               "None")
call <sid>highlight("SpecialKey",   "None",               "None",               "None")
call <sid>highlight("TooLong",      "None",               "None",               "None")
call <sid>highlight("Underlined",   "None",               "None",               "None")
call <sid>highlight("Visual",       s:bg_region,          s:fg_main,            "None")
call <sid>highlight("VisualNOS",    "None",               "None",               "None")
call <sid>highlight("WarningMsg",   "None",               "None",               "None")
call <sid>highlight("WildMenu",     "None",               "None",               "None")
call <sid>highlight("Title",        "None",               "None",               "None")
call <sid>highlight("Conceal",      "None",               "None",               "None")
call <sid>highlight("Cursor",       "None",               "None",               "None")
call <sid>highlight("NonText",      "None",               "None",               "None")
call <sid>highlight("LineNr",       s:bg_dim,             "None",               "None")
call <sid>highlight("SignColumn",   "None",               "None",               "None")
call <sid>highlight("StatusLine",   s:bg_active,          "None",               "None")
call <sid>highlight("StatusLineNC", s:bg_inactive,        "None",               "None")
call <sid>highlight("VertSplit",    "None",               "None",               "None")
call <sid>highlight("ColorColumn",  "None",               "None",               "None")
call <sid>highlight("CursorColumn", "None",               "None",               "None")
call <sid>highlight("CursorLine",   s:bg_hl_line,         "None",               "None")
call <sid>highlight("CursorLineNr", s:bg_active,          "None",               "bold")
call <sid>highlight("QuickFixLine", "None",               "None",               "None")
call <sid>highlight("PMenu",        "None",               "None",               "None")
call <sid>highlight("PMenuSel",     "None",               "None",               "None")
call <sid>highlight("TabLine",      "None",               "None",               "None")
call <sid>highlight("TabLineFill",  "None",               "None",               "None")
call <sid>highlight("TabLineSel",   "None",               "None",               "None")

" Standard syntax highlighting
call <sid>highlight("Boolean",      "None",               "None",               "None")
call <sid>highlight("Character",    "None",               "None",               "None")
call <sid>highlight("Comment",      "None",               s:fg_alt,             "None")
call <sid>highlight("Conditional",  "None",               s:magenta_alt_other,  "None")
call <sid>highlight("Constant",     "None",               "None",               "None")
call <sid>highlight("Define",       "None",               "None",               "None")
call <sid>highlight("Delimiter",    "None",               "None",               "None")
call <sid>highlight("Float", "       None",               "None",               "None")
call <sid>highlight("Function",     "None",               s:magenta_alt_other,  "None")
call <sid>highlight("Identifier",   "None",               "None",               "None")
call <sid>highlight("Include",      "None",               s:red,                "None")
call <sid>highlight("Keyword",      "None",               "None",               "None")
call <sid>highlight("Label",        "None",               "None",               "None")
call <sid>highlight("Number",       "None",               "None",               "None")
call <sid>highlight("Operator",     "None",               s:magenta,            "None")
call <sid>highlight("PreProc",      "None",               "None",               "None")
call <sid>highlight("Repeat",       "None",               s:magenta_alt_other,  "None")
call <sid>highlight("Special",      "None",               "None",               "None")
call <sid>highlight("SpecialChar",  "None",               s:magenta,            "None")
call <sid>highlight("Statement",    "None",               "None",               "None")
call <sid>highlight("StorageClass", "None",               s:magenta_alt_other,  "None")
call <sid>highlight("String",       "None",               s:blue_alt,           "None")
call <sid>highlight("Structure",    "None",               "None",               "None")
call <sid>highlight("Tag",          "None",               "None",               "None")
call <sid>highlight("Todo",         "None",               "None",               "None")
call <sid>highlight("Type",         "None",               s:cyan,               "None")
call <sid>highlight("Typedef",      "None",               "None",               "None")

" Matlab highlighting
" TODO(chlyz): Improve highlight for `...`

" Tree sitter highlighting
call <sid>highlight("TSConstant",        "None", s:blue,              "None")
call <sid>highlight("TSFunction",        "None", s:magenta_faint,     "None")
call <sid>highlight("TSFuncMacro",       "None", s:red,               "None")
call <sid>highlight("TSInclude",         "None", s:red,               "None")
call <sid>highlight("TSKeyword",         "None", s:magenta_alt_other, "None")
call <sid>highlight("TSKeywordFunction", "None", s:magenta_alt_other, "None")
call <sid>highlight("TSNote",            "None", s:cyan,              "None")
call <sid>highlight("TSNumber",          "None", s:blue,              "None")
call <sid>highlight("TSWarning",         "None", s:yellow,            "None")
call <sid>highlight("TSType",            "None", s:cyan_alt_other,    "None")
call <sid>highlight("TSVariable",        "None", s:cyan,              "None")

" C highlighting
call <sid>highlight("cOperator",  "None", "None", "None")
call <sid>highlight("cPreCondit", "None", s:red, "None")
call <sid>highlight("cType",      "None", s:cyan, "None")

" C# highlighting
call <sid>highlight("csClass",                "None", "None", "None")
call <sid>highlight("csAttribute",            "None", "None", "None")
call <sid>highlight("csModifier",             "None", "None", "None")
call <sid>highlight("csType",                 "None", "None", "None")
call <sid>highlight("csUnspecifiedStatement", "None", "None", "None")
call <sid>highlight("csContextualStatement",  "None", "None", "None")
call <sid>highlight("csNewDecleration",       "None", "None", "None")

" CSS highlighting
call <sid>highlight("cssBraces",    "None", "None", "None")
call <sid>highlight("cssClassName", "None", "None", "None")
call <sid>highlight("cssColor",     "None", "None", "None")


" Git highlighting
call <sid>highlight("gitcommitOverflow",          "None", "None", "None")
call <sid>highlight("gitcommitSummary",           "None", "None", "None")
call <sid>highlight("gitcommitComment",           "None", "None", "None")
call <sid>highlight("gitcommitUntracked",         "None", "None", "None")
call <sid>highlight("gitcommitDiscarded",         "None", "None", "None")
call <sid>highlight("gitcommitSelected",          "None", "None", "None")
call <sid>highlight("gitcommitHeader",            "None", "None", "None")
call <sid>highlight("gitcommitSelectedType",      "None", "None", "None")
call <sid>highlight("gitcommitUnmergedType",      "None", "None", "None")
call <sid>highlight("gitcommitDiscardedType",     "None", "None", "None")
call <sid>highlight("gitcommitBranch",            "None", "None", "None")
call <sid>highlight("gitcommitUntrackedFile",     "None", "None", "None")
call <sid>highlight("gitcommitUnmergedFile",      "None", "None", "None")
call <sid>highlight("gitcommitDiscardedFile",     "None", "None", "None")
call <sid>highlight("gitcommitSelectedFile",      "None", "None", "None")

" GitGutter highlighting
call <sid>highlight("GitGutterAdd",               "None", "None", "None")
call <sid>highlight("GitGutterChange",            "None", "None", "None")
call <sid>highlight("GitGutterDelete",            "None", "None", "None")
call <sid>highlight("GitGutterChangeDelete",      "None", "None", "None")

" HTML highlighting
call <sid>highlight("htmlBold",                   "None", "None", "None")
call <sid>highlight("htmlItalic",                 "None", "None", "None")
call <sid>highlight("htmlEndTag",                 "None", "None", "None")
call <sid>highlight("htmlTag",                    "None", "None", "None")

" JavaScript highlighting
call <sid>highlight("javaScript",                 "None", "None", "None")
call <sid>highlight("javaScriptBraces",           "None", "None", "None")
call <sid>highlight("javaScriptNumber",           "None", "None", "None")

" pangloss/vim-javascript highlighting
call <sid>highlight("jsOperator",                 "None", "None", "None")
call <sid>highlight("jsStatement",                "None", "None", "None")
call <sid>highlight("jsReturn",                   "None", "None", "None")
call <sid>highlight("jsThis",                     "None", "None", "None")
call <sid>highlight("jsClassDefinition",          "None", "None", "None")
call <sid>highlight("jsFunction",                 "None", "None", "None")
call <sid>highlight("jsFuncName",                 "None", "None", "None")
call <sid>highlight("jsFuncCall",                 "None", "None", "None")
call <sid>highlight("jsClassFuncName",            "None", "None", "None")
call <sid>highlight("jsClassMethodType",          "None", "None", "None")
call <sid>highlight("jsRegexpString",             "None", "None", "None")
call <sid>highlight("jsGlobalObjects",            "None", "None", "None")
call <sid>highlight("jsGlobalNodeObjects",        "None", "None", "None")
call <sid>highlight("jsExceptions"  ,             "None", "None", "None")
call <sid>highlight("jsBuiltins",                 "None", "None", "None")

" Mail highlighting
call <sid>highlight("mailQuoted1",                "None", "None", "None")
call <sid>highlight("mailQuoted2",                "None", "None", "None")
call <sid>highlight("mailQuoted3",                "None", "None", "None")
call <sid>highlight("mailQuoted4",                "None", "None", "None")
call <sid>highlight("mailQuoted5",                "None", "None", "None")
call <sid>highlight("mailQuoted6",                "None", "None", "None")
call <sid>highlight("mailURL",                    "None", "None", "None")
call <sid>highlight("mailEmail",                  "None", "None", "None")

" Markdown highlighting
call <sid>highlight("markdownCode",               "None", "None", "None")
call <sid>highlight("markdownError",              "None", "None", "None")
call <sid>highlight("markdownCodeBlock",          "None", "None", "None")
call <sid>highlight("markdownHeadingDelimiter",   "None", "None", "None")

" NERDTree highlighting
call <sid>highlight("NERDTreeDirSlash",           "None", "None", "None")
call <sid>highlight("NERDTreeExecFile",           "None", "None", "None")

" PHP highlighting
call <sid>highlight("phpMemberSelector",          "None", "None", "None")
call <sid>highlight("phpComparison",              "None", "None", "None")
call <sid>highlight("phpParent",                  "None", "None", "None")
call <sid>highlight("phpMethodsVar",              "None", "None", "None")

" Python highlighting
call <sid>highlight("pythonOperator",             "None", "None", "None")
call <sid>highlight("pythonRepeat",               "None", "None", "None")
call <sid>highlight("pythonInclude",              "None", "None", "None")
call <sid>highlight("pythonStatement",            "None", "None", "None")

" Ruby highlighting
call <sid>highlight("rubyAttribute",              "None", "None", "None")
call <sid>highlight("rubyConstant",               "None", "None", "None")
call <sid>highlight("rubyInterpolationDelimiter", "None", "None", "None")
call <sid>highlight("rubyRegexp",                 "None", "None", "None")
call <sid>highlight("rubySymbol",                 "None", "None", "None")
call <sid>highlight("rubyStringDelimiter",        "None", "None", "None")

" SASS highlighting
call <sid>highlight("sassidChar",                 "None", "None", "None")
call <sid>highlight("sassClassChar",              "None", "None", "None")
call <sid>highlight("sassInclude",                "None", "None", "None")
call <sid>highlight("sassMixing",                 "None", "None", "None")
call <sid>highlight("sassMixinName",              "None", "None", "None")

" Signify highlighting
call <sid>highlight("SignifySignAdd",             "None", "None", "None")
call <sid>highlight("SignifySignChange",          "None", "None", "None")
call <sid>highlight("SignifySignDelete",          "None", "None", "None")

" Spelling highlighting
call <sid>highlight("SpellBad",   "None", "None", "None")
call <sid>highlight("SpellLocal", "None", "None", "None")
call <sid>highlight("SpellCap",   "None", "None", "None")
call <sid>highlight("SpellRare",  "None", "None", "None")

" Startify highlighting
call <sid>highlight("StartifyBracket", "None", "None", "None")
call <sid>highlight("StartifyFile",    "None", "None", "None")
call <sid>highlight("StartifyFooter",  "None", "None", "None")
call <sid>highlight("StartifyHeader",  "None", "None", "None")
call <sid>highlight("StartifyNumber",  "None", "None", "None")
call <sid>highlight("StartifyPath",    "None", "None", "None")
call <sid>highlight("StartifySection", "None", "None", "None")
call <sid>highlight("StartifySelect",  "None", "None", "None")
call <sid>highlight("StartifySlash",   "None", "None", "None")
call <sid>highlight("StartifySpecial", "None", "None", "None")

" Java highlighting
call <sid>highlight("javaOperator", "None", "None", "None")

" Netrw highlight
highlight link netrwDir Directory

" Remove functions

" Remove color variables
" unlet s:gui00 s:gui01 s:gui02 s:gui03  s:gui04  s:gui05  s:gui06  s:gui07  s:gui08  s:gui09 s:gui0A  s:gui0B  s:gui0C  s:gui0D  s:gui0E  s:gui0F
" unlet s:cterm00 s:cterm01 s:cterm02 s:cterm03 s:cterm04 s:cterm05 s:cterm06 s:cterm07 s:cterm08 s:cterm09 s:cterm0A s:cterm0B s:cterm0C s:cterm0D s:cterm0E s:cterm0F

unlet s:bg_main s:fg_main s:bg_dim s:fg_dim s:bg_alt s:fg_alt
unlet s:bg_hl_line
unlet s:bg_active s:bg_inactive
unlet s:bg_region s:bg_special_mild
unlet s:bg_paren_match s:bg_paren_match_intense
unlet s:bg_search s:bg_incsearch
unlet s:red s:red_alt s:red_alt_other s:red_faint s:red_alt_faint s:red_alt_other_faint
unlet s:red_subtle_bg s:red_intense_bg
unlet s:cyan s:cyan_alt s:cyan_alt_other s:cyan_faint s:cyan_alt_faint s:cyan_alt_other_faint
unlet s:cyan_active s:cyan_subtle_bg s:cyan_intense_bg
unlet s:green s:green_alt s:green_alt_other s:green_faint s:green_alt_faint s:green_alt_other_faint
unlet s:green_subtle_bg s:green_intense_bg s:green_refine_bg
unlet s:blue s:blue_alt s:blue_alt_other s:blue_faint s:blue_alt_faint s:blue_alt_other_faint
unlet s:blue_subtle_bg s:blue_intense_bg
unlet s:yellow s:yellow_alt s:yellow_alt_other s:yellow_faint s:yellow_alt_faint s:yellow_alt_other_faint
unlet s:yellow_subtle_bg s:yellow_intense_bg
unlet s:magenta s:magenta_alt s:magenta_alt_other s:magenta_faint s:magenta_alt_faint s:magenta_alt_other_faint

unlet s:red_intense s:orange_intense s:green_intense s:yellow_intense s:blue_intense s:magenta_intense s:purple_intense s:cyan_intense

" Remove functions
delf <sid>highlight

" call <sid>hi("Normal",        s:bg_main, s:fg_main, "", "", "", "")
