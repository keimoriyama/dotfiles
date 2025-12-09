-- lua_source {{{
-- local current_dir = vim.fn.getcwd()
-- if string.match(current_dir, "Atcoder") == nil then
-- 	require("copilot").setup({
-- 		panel = {
-- 			enabled = false,
-- 		},
-- 		suggestion = {
-- 			enabled = true,
-- 			auto_trigger = true,
-- 			keymap = {
-- 				accept = "<C-c>",
-- 			},
-- 		},
-- 	})
-- end
require("copilot").setup({
	panel = {
		enabled = false,
	},
	suggestion = {
		enabled = true,
		auto_trigger = true,
	},
})
vim.g.copilot_no_tab_map = false
print(vim.b.copilot_enabled)

-- }}}
