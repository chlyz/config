set background=dark
highlight clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name="Modus Vivendi"

let s:bg_main           = "#000000"
let s:fg_main           = "#ffffff"
let s:bg_dim            = "#100f10"
let s:bg_alt            = "#f0f0f0"
let s:fg_alt            = "#a8a8a8"
let s:bg_hl_line        = "#151823"
let s:bg_region         = "#3c3c3c"
let s:bg_paren_match    = "#6f3355"
let s:bg_active         = "#323232"
let s:bg_inactive       = "#efefef"
let s:red               = "#ff8059"
let s:red_alt_other     = "#ff9077"
let s:green             = "#44bc44"
let s:yellow            = "#d0bc00"
let s:blue              = "#2fafff"
let s:blue_alt          = "#79a8ff"
let s:blue_alt_other    = "#00bcff"
let s:magenta           = "#feacd0"
let s:magenta_alt_other = "#b6a0ff"
let s:magenta_faint     = "#e0b2d6"
let s:cyan              = "#00d3d0"
let s:cyan_alt_other    = "#6ae4b9"

" These are not in the proper theme but emacs applies them somewhere.
let s:bg_search    = "#004065"
let s:bg_incsearch = "#ee6a50"

let s:bg_diff_heading               = "#304466"
let s:fg_diff_heading               = "#dae7ff"
let s:bg_diff_added_deuteran        = "#001a3f"
let s:fg_diff_added_deuteran        = "#c4cdf2"
let s:bg_diff_changed               = "#2a2000"
let s:fg_diff_changed               = "#b0ba9f"
let s:bg_diff_removed               = "#40160f"
let s:fg_diff_removed               = "#c6adaa"

" Modus theme highlighting function
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
" call <SID>highlight("DiffFile",    s:bg_diff_heading, s:fg_diff_heading, "None")
" call <SID>highlight("DiffNewFile", s:red, "None", "None")

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
call <SID>highlight("Normal",       s:bg_main,            s:fg_main,            "None")
call <SID>highlight("Bold",         "None",               "None",               "bold")
call <SID>highlight("Debug",        "None",               "None",               "None")
call <SID>highlight("Directory",    "None",               s:blue,               "bold")
call <SID>highlight("Error",        "None",               "None",               "None")
call <SID>highlight("ErrorMsg",     "None",               "None",               "None")
call <SID>highlight("Exception",    "None",               "None",               "None")
call <SID>highlight("FoldColumn",   "None",               "None",               "None")
call <SID>highlight("Folded",       "None",               "None",               "None")
call <SID>highlight("IncSearch",    s:bg_incsearch,       s:bg_main,            "bold")
call <SID>highlight("Italic",       "None",               "None",               "italic")
call <SID>highlight("Macro",        "None",               "None",               "None")
call <SID>highlight("MatchParen",   s:bg_paren_match,     "None",               "None")
call <SID>highlight("ModeMsg",      "None",               "None",               "None")
call <SID>highlight("MoreMsg",      "None",               "None",               "None")
call <SID>highlight("Question",     "None",               "None",               "None")
call <SID>highlight("Search",       s:bg_search,          s:bg_main,            "None")
call <SID>highlight("Substitute",   "None",               "None",               "None")
call <SID>highlight("SpecialKey",   "None",               "None",               "None")
call <SID>highlight("TooLong",      "None",               "None",               "None")
call <SID>highlight("Underlined",   "None",               "None",               "None")
call <SID>highlight("Visual",       s:bg_region,          s:fg_main,            "None")
call <SID>highlight("VisualNOS",    "None",               "None",               "None")
call <SID>highlight("WarningMsg",   "None",               "None",               "None")
call <SID>highlight("WildMenu",     "None",               "None",               "None")
call <SID>highlight("Title",        "None",               "None",               "None")
call <SID>highlight("Conceal",      "None",               "None",               "None")
call <SID>highlight("Cursor",       "None",               "None",               "None")
call <SID>highlight("NonText",      "None",               "None",               "None")
call <SID>highlight("LineNr",       s:bg_dim,             "None",               "None")
call <SID>highlight("SignColumn",   "None",               "None",               "None")
call <SID>highlight("StatusLine",   s:bg_active,          "None",               "None")
call <SID>highlight("StatusLineNC", s:bg_inactive,        "None",               "None")
call <SID>highlight("VertSplit",    "None",               "None",               "None")
call <SID>highlight("ColorColumn",  "None",               "None",               "None")
call <SID>highlight("CursorColumn", "None",               "None",               "None")
call <SID>highlight("CursorLine",   s:bg_hl_line,         "None",               "None")
call <SID>highlight("CursorLineNr", s:bg_active,          "None",               "bold")
call <SID>highlight("QuickFixLine", "None",               "None",               "None")
call <SID>highlight("PMenu",        "None",               "None",               "None")
call <SID>highlight("PMenuSel",     "None",               "None",               "None")
call <SID>highlight("TabLine",      "None",               "None",               "None")
call <SID>highlight("TabLineFill",  "None",               "None",               "None")
call <SID>highlight("TabLineSel",   "None",               "None",               "None")

" Standard syntax highlighting
call <SID>highlight("Boolean",      "None",               "None",               "None")
call <SID>highlight("Character",    "None",               "None",               "None")
call <SID>highlight("Comment",      "None",               s:fg_alt,             "None")
call <SID>highlight("Conditional",  "None",               s:magenta_alt_other,  "None")
call <SID>highlight("Constant",     "None",               "None",               "None")
call <SID>highlight("Define",       "None",               "None",               "None")
call <SID>highlight("Delimiter",    "None",               "None",               "None")
call <SID>highlight("Float", "       None",               "None",               "None")
call <SID>highlight("Function",     "None",               s:magenta,  "None")
call <SID>highlight("Identifier",   "None",               "None",               "None")
call <SID>highlight("Include",      "None",               s:red,                "None")
call <SID>highlight("Keyword",      "None",               "None",               "None")
call <SID>highlight("Label",        "None",               "None",               "None")
call <SID>highlight("Number",       "None",               "None",               "None")
call <SID>highlight("Operator",     "None",               s:magenta,            "None")
call <SID>highlight("PreProc",      "None",               "None",               "None")
call <SID>highlight("Repeat",       "None",               s:magenta_alt_other,  "None")
call <SID>highlight("Special",      "None",               "None",               "None")
call <SID>highlight("SpecialChar",  "None",               s:magenta,            "None")
call <SID>highlight("Statement",    "None",               "None",               "None")
call <SID>highlight("StorageClass", "None",               s:magenta_alt_other,  "None")
call <SID>highlight("String",       "None",               s:blue_alt,           "None")
call <SID>highlight("Structure",    "None",               "None",               "None")
call <SID>highlight("Tag",          "None",               "None",               "None")
call <SID>highlight("Todo",         "None",               "None",               "None")
call <SID>highlight("Type",         "None",               s:cyan,               "None")
call <SID>highlight("Typedef",      "None",               "None",               "None")

" Matlab highlighting
" TODO(chlyz): Improve highlight for `...`

" Tree sitter highlighting
call <SID>highlight("TSConstant",        "None", s:blue,              "None")
call <SID>highlight("TSFunction",        "None", s:magenta,           "None")
call <SID>highlight("TSFuncMacro",       "None", s:red_alt_other,     "bold")
call <SID>highlight("TSInclude",         "None", s:red_alt_other,     "bold")
call <SID>highlight("TSKeyword",         "None", s:red_alt_other,     "bold")
call <SID>highlight("TSKeywordFunction", "None", s:magenta_alt_other, "None")
call <SID>highlight("TSKeywordOperator", "None", s:magenta_alt_other, "None")
call <SID>highlight("TSNote",            "None", s:cyan,              "None")
call <SID>highlight("TSNumber",          "None", s:blue,              "None")
call <SID>highlight("TSWarning",         "None", s:yellow,            "None")
call <SID>highlight("TSType",            "None", s:cyan_alt_other,    "None")
call <SID>highlight("TSVariable",        "None", s:cyan,              "None")
call <SID>highlight("TSVariableBuiltin", "None", s:cyan,              "None")

" C highlighting
call <SID>highlight("cOperator",  "None", "None", "None")
call <SID>highlight("cPreCondit", "None", s:red, "None")
call <SID>highlight("cType",      "None", s:cyan, "None")

" C# highlighting
call <SID>highlight("csClass",                "None", "None", "None")
call <SID>highlight("csAttribute",            "None", "None", "None")
call <SID>highlight("csModifier",             "None", "None", "None")
call <SID>highlight("csType",                 "None", "None", "None")
call <SID>highlight("csUnspecifiedStatement", "None", "None", "None")
call <SID>highlight("csContextualStatement",  "None", "None", "None")
call <SID>highlight("csNewDecleration",       "None", "None", "None")

" CSS highlighting
call <SID>highlight("cssBraces",    "None", "None", "None")
call <SID>highlight("cssClassName", "None", "None", "None")
call <SID>highlight("cssColor",     "None", "None", "None")


" Git highlighting
call <SID>highlight("gitcommitOverflow",          "None", "None", "None")
call <SID>highlight("gitcommitSummary",           "None", "None", "None")
call <SID>highlight("gitcommitComment",           "None", "None", "None")
call <SID>highlight("gitcommitUntracked",         "None", "None", "None")
call <SID>highlight("gitcommitDiscarded",         "None", "None", "None")
call <SID>highlight("gitcommitSelected",          "None", "None", "None")
call <SID>highlight("gitcommitHeader",            "None", "None", "None")
call <SID>highlight("gitcommitSelectedType",      "None", "None", "None")
call <SID>highlight("gitcommitUnmergedType",      "None", "None", "None")
call <SID>highlight("gitcommitDiscardedType",     "None", "None", "None")
call <SID>highlight("gitcommitBranch",            "None", "None", "None")
call <SID>highlight("gitcommitUntrackedFile",     "None", "None", "None")
call <SID>highlight("gitcommitUnmergedFile",      "None", "None", "None")
call <SID>highlight("gitcommitDiscardedFile",     "None", "None", "None")
call <SID>highlight("gitcommitSelectedFile",      "None", "None", "None")

" GitGutter highlighting
call <SID>highlight("GitGutterAdd",               "None", "None", "None")
call <SID>highlight("GitGutterChange",            "None", "None", "None")
call <SID>highlight("GitGutterDelete",            "None", "None", "None")
call <SID>highlight("GitGutterChangeDelete",      "None", "None", "None")

" HTML highlighting
call <SID>highlight("htmlBold",                   "None", "None", "None")
call <SID>highlight("htmlItalic",                 "None", "None", "None")
call <SID>highlight("htmlEndTag",                 "None", "None", "None")
call <SID>highlight("htmlTag",                    "None", "None", "None")

" JavaScript highlighting
call <SID>highlight("javaScript",                 "None", "None", "None")
call <SID>highlight("javaScriptBraces",           "None", "None", "None")
call <SID>highlight("javaScriptNumber",           "None", "None", "None")

" pangloss/vim-javascript highlighting
call <SID>highlight("jsOperator",                 "None", "None", "None")
call <SID>highlight("jsStatement",                "None", "None", "None")
call <SID>highlight("jsReturn",                   "None", "None", "None")
call <SID>highlight("jsThis",                     "None", "None", "None")
call <SID>highlight("jsClassDefinition",          "None", "None", "None")
call <SID>highlight("jsFunction",                 "None", "None", "None")
call <SID>highlight("jsFuncName",                 "None", "None", "None")
call <SID>highlight("jsFuncCall",                 "None", "None", "None")
call <SID>highlight("jsClassFuncName",            "None", "None", "None")
call <SID>highlight("jsClassMethodType",          "None", "None", "None")
call <SID>highlight("jsRegexpString",             "None", "None", "None")
call <SID>highlight("jsGlobalObjects",            "None", "None", "None")
call <SID>highlight("jsGlobalNodeObjects",        "None", "None", "None")
call <SID>highlight("jsExceptions"  ,             "None", "None", "None")
call <SID>highlight("jsBuiltins",                 "None", "None", "None")

" Mail highlighting
call <SID>highlight("mailQuoted1",                "None", "None", "None")
call <SID>highlight("mailQuoted2",                "None", "None", "None")
call <SID>highlight("mailQuoted3",                "None", "None", "None")
call <SID>highlight("mailQuoted4",                "None", "None", "None")
call <SID>highlight("mailQuoted5",                "None", "None", "None")
call <SID>highlight("mailQuoted6",                "None", "None", "None")
call <SID>highlight("mailURL",                    "None", "None", "None")
call <SID>highlight("mailEmail",                  "None", "None", "None")

" Markdown highlighting
call <SID>highlight("markdownCode",               "None", "None", "None")
call <SID>highlight("markdownError",              "None", "None", "None")
call <SID>highlight("markdownCodeBlock",          "None", "None", "None")
call <SID>highlight("markdownHeadingDelimiter",   "None", "None", "None")

" NERDTree highlighting
call <SID>highlight("NERDTreeDirSlash",           "None", "None", "None")
call <SID>highlight("NERDTreeExecFile",           "None", "None", "None")

" PHP highlighting
call <SID>highlight("phpMemberSelector",          "None", "None", "None")
call <SID>highlight("phpComparison",              "None", "None", "None")
call <SID>highlight("phpParent",                  "None", "None", "None")
call <SID>highlight("phpMethodsVar",              "None", "None", "None")

" Python highlighting
call <SID>highlight("pythonOperator",             "None", "None", "None")
call <SID>highlight("pythonRepeat",               "None", "None", "None")
call <SID>highlight("pythonInclude",              "None", "None", "None")
call <SID>highlight("pythonStatement",            "None", "None", "None")

" Ruby highlighting
call <SID>highlight("rubyAttribute",              "None", "None", "None")
call <SID>highlight("rubyConstant",               "None", "None", "None")
call <SID>highlight("rubyInterpolationDelimiter", "None", "None", "None")
call <SID>highlight("rubyRegexp",                 "None", "None", "None")
call <SID>highlight("rubySymbol",                 "None", "None", "None")
call <SID>highlight("rubyStringDelimiter",        "None", "None", "None")

" SASS highlighting
call <SID>highlight("sassidChar",                 "None", "None", "None")
call <SID>highlight("sassClassChar",              "None", "None", "None")
call <SID>highlight("sassInclude",                "None", "None", "None")
call <SID>highlight("sassMixing",                 "None", "None", "None")
call <SID>highlight("sassMixinName",              "None", "None", "None")

" Signify highlighting
call <SID>highlight("SignifySignAdd",             "None", "None", "None")
call <SID>highlight("SignifySignChange",          "None", "None", "None")
call <SID>highlight("SignifySignDelete",          "None", "None", "None")

" Spelling highlighting
call <SID>highlight("SpellBad",   "None", "None", "None")
call <SID>highlight("SpellLocal", "None", "None", "None")
call <SID>highlight("SpellCap",   "None", "None", "None")
call <SID>highlight("SpellRare",  "None", "None", "None")

" Startify highlighting
call <SID>highlight("StartifyBracket", "None", "None", "None")
call <SID>highlight("StartifyFile",    "None", "None", "None")
call <SID>highlight("StartifyFooter",  "None", "None", "None")
call <SID>highlight("StartifyHeader",  "None", "None", "None")
call <SID>highlight("StartifyNumber",  "None", "None", "None")
call <SID>highlight("StartifyPath",    "None", "None", "None")
call <SID>highlight("StartifySection", "None", "None", "None")
call <SID>highlight("StartifySelect",  "None", "None", "None")
call <SID>highlight("StartifySlash",   "None", "None", "None")
call <SID>highlight("StartifySpecial", "None", "None", "None")

" Java highlighting
call <SID>highlight("javaOperator", "None", "None", "None")

" Netrw highlight
highlight link netrwDir Directory

" Remove functions

" Remove color variables
" unlet s:gui00 s:gui01 s:gui02 s:gui03  s:gui04  s:gui05  s:gui06  s:gui07  s:gui08  s:gui09 s:gui0A  s:gui0B  s:gui0C  s:gui0D  s:gui0E  s:gui0F
" unlet s:cterm00 s:cterm01 s:cterm02 s:cterm03 s:cterm04 s:cterm05 s:cterm06 s:cterm07 s:cterm08 s:cterm09 s:cterm0A s:cterm0B s:cterm0C s:cterm0D s:cterm0E s:cterm0F

unlet s:bg_main s:fg_main s:bg_dim s:bg_alt s:fg_alt
unlet s:bg_hl_line
unlet s:bg_active s:bg_inactive
unlet s:bg_region
unlet s:bg_paren_match
unlet s:bg_search s:bg_incsearch
unlet s:red
unlet s:cyan s:cyan_alt_other
unlet s:green
unlet s:blue s:blue_alt
unlet s:yellow
unlet s:magenta s:magenta_alt_other s:magenta_faint


" Remove functions
delf <SID>highlight

" call <SID>hi("Normal",        s:bg_main, s:fg_main, "", "", "", "")
