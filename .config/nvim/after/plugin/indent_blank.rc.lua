vim.opt.list = true
vim.opt.listchars:append "space: "

local status, indent_blankline = pcall(require, 'indent_blankline')
if not status then
  return
end
indent_blankline.setup {
	pace_char_blankline = " ",
	how_current_context = true,
	how_current_context_start = true,
	how_end_of_line = true,
}
