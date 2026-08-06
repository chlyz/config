vim.cmd.highlight("clear")
if vim.fn.exists("syntax_on") == 1 then
  vim.cmd.syntax("reset")
end

vim.g.colors_name = "modus_vivendi_tinted"

local palette = {
  bg_main = "#0d0e1c", bg_dim = "#1d2235", fg_main = "#ffffff", fg_dim = "#989898",
  fg_alt = "#c6daff", bg_active = "#4a4f69", bg_inactive = "#2b3045", border = "#61647a",
  red = "#ff5f59", red_faint = "#ef8386", red_cooler = "#ff7f86",
  green_cooler = "#11c777", yellow = "#d0bc00", yellow_faint = "#d2b580",
  yellow_warmer = "#fec43f", blue = "#2fafff", blue_warmer = "#79a8ff",
  blue_cooler = "#00bcff", magenta = "#feacd0", magenta_warmer = "#f78fe7",
  magenta_cooler = "#b6a0ff", magenta_intense = "#ff66ff", cyan = "#00d3d0",
  cyan_warmer = "#4ae2f0", cyan_cooler = "#6ae4b9", cyan_faint = "#9ac8e0",
  bg_completion = "#483d8a", bg_hover = "#45605e", bg_hl_line = "#303a6f", bg_region = "#555a66",
  bg_added = "#003a2f", fg_added = "#a0e0a0", bg_changed = "#363300", fg_changed = "#efef80",
  bg_removed = "#4f1127", fg_removed = "#ffbfbf", bg_diff_context = "#1a1f30",
  bg_error = "#9d1f1f", bg_warning = "#7a6100", bg_note = "#2266ae",
}

local function hi(group, spec)
  vim.api.nvim_set_hl(0, group, spec)
end

hi("Normal", { fg = palette.fg_main, bg = palette.bg_main })
hi("NormalFloat", { fg = palette.fg_main, bg = palette.bg_dim })
hi("FloatBorder", { fg = palette.border, bg = palette.bg_dim })
hi("Cursor", { fg = palette.bg_main, bg = palette.magenta_intense })
hi("CursorLine", { bg = palette.bg_hl_line })
hi("CursorColumn", { bg = palette.bg_hl_line })
hi("ColorColumn", { bg = palette.bg_dim })
hi("Visual", { fg = palette.fg_main, bg = palette.bg_region })
hi("Search", { fg = palette.fg_main, bg = palette.bg_warning })
hi("IncSearch", { fg = palette.fg_main, bg = palette.bg_note })
hi("Substitute", { fg = palette.fg_main, bg = palette.bg_error })
hi("LineNr", { fg = palette.fg_dim, bg = palette.bg_dim })
hi("CursorLineNr", { fg = palette.fg_main, bg = palette.bg_active, bold = true })
hi("SignColumn", { fg = palette.fg_dim, bg = palette.bg_dim })
hi("StatusLine", { fg = palette.fg_main, bg = "#484d67" })
hi("StatusLineNC", { fg = "#969696", bg = "#292d48" })
hi("WinSeparator", { fg = palette.border })
hi("Pmenu", { fg = palette.fg_main, bg = palette.bg_completion })
hi("PmenuSel", { fg = palette.fg_main, bg = palette.bg_hover })
hi("PmenuSbar", { bg = palette.bg_inactive })
hi("PmenuThumb", { bg = palette.border })
hi("TabLine", { fg = palette.fg_dim, bg = palette.bg_inactive })
hi("TabLineSel", { fg = palette.fg_main, bg = palette.bg_main })

hi("Comment", { fg = palette.red_faint, italic = true })
hi("Constant", { fg = palette.magenta_cooler })
hi("String", { fg = palette.blue })
hi("Character", { fg = palette.blue })
hi("Number", { fg = palette.fg_main })
hi("Boolean", { fg = palette.magenta_cooler })
hi("Identifier", { fg = palette.cyan_warmer })
hi("Function", { fg = palette.magenta_warmer })
hi("Statement", { fg = palette.blue_warmer })
hi("Keyword", { fg = palette.blue_warmer })
hi("Operator", { fg = palette.fg_main })
hi("PreProc", { fg = palette.red_cooler })
hi("Type", { fg = palette.green_cooler })
hi("Special", { fg = palette.magenta_warmer })
hi("Delimiter", { fg = palette.fg_main })
hi("Error", { fg = palette.fg_main, bg = palette.bg_error })
hi("Todo", { fg = palette.fg_main, bg = palette.bg_warning, bold = true })

hi("DiagnosticError", { fg = palette.red })
hi("DiagnosticWarn", { fg = palette.yellow })
hi("DiagnosticInfo", { fg = palette.green_cooler })
hi("DiagnosticHint", { fg = palette.cyan })
hi("DiagnosticUnderlineError", { undercurl = true, sp = palette.red })
hi("DiagnosticUnderlineWarn", { undercurl = true, sp = palette.yellow })
hi("DiagnosticUnderlineInfo", { undercurl = true, sp = palette.cyan })
hi("DiagnosticUnderlineHint", { undercurl = true, sp = palette.cyan })

hi("DiffAdd", { fg = palette.fg_added, bg = palette.bg_added })
hi("DiffChange", { fg = palette.fg_changed, bg = palette.bg_changed })
hi("DiffDelete", { fg = palette.fg_removed, bg = palette.bg_removed })
hi("DiffText", { fg = palette.fg_main, bg = "#035542", bold = true })
hi("diffAdded", { fg = palette.fg_added })
hi("diffChanged", { fg = palette.fg_changed })
hi("diffRemoved", { fg = palette.fg_removed })

local links = {
  ["@comment"] = "Comment", ["@string"] = "String", ["@character"] = "Character",
  ["@number"] = "Number", ["@boolean"] = "Boolean", ["@constant"] = "Constant",
  ["@variable"] = "Identifier", ["@variable.member"] = "Identifier",
  ["@function"] = "Function", ["@function.call"] = "Function",
  ["@method"] = "Function", ["@method.call"] = "Function",
  ["@keyword"] = "Keyword", ["@keyword.function"] = "Keyword",
  ["@keyword.import"] = "PreProc", ["@type"] = "Type", ["@type.builtin"] = "Type",
  ["@property"] = "Identifier", ["@attribute"] = "PreProc",
  ["@operator"] = "Operator", ["@punctuation"] = "Delimiter",
  ["@punctuation.bracket"] = "Delimiter", ["@tag"] = "Type", ["@tag.attribute"] = "Identifier",
}
for group, target in pairs(links) do
  hi(group, { link = target })
end
