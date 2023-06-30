local status, lspsaga = pcall(require, "lspsaga")
if not status then
	return
end
lspsaga.setup({
	border_style = "single",
	symbol_in_winbar = {
		enable = true,
	},
	code_action_lightbulb = {
		enable = true,
	},
	show_outline = {
		win_width = 50,
		auto_preview = false,
	},
	symbol_in_winbar = {
		enable = true,
	},
})
