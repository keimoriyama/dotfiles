-- lua_source {{{
local current_dir = vim.fn.getcwd()
if string.match(current_dir, "Atcoder") == nil then
	require("copilot").setup({
		panel = {
			enabled = false,
		},
		suggestion = {
			enabled = true,
			auto_trigger = true,
			keymap = {
				accept = "<C-c>",
			},
		},
	})
	vim.keymap.set("i", "<C-c><C-t>", "<cmd>Copilot toggle<cr>")
end
-- }}}
