---@type LazySpec
local spec = {
	{
		"zbirenbaum/copilot.lua",
		event = 'InsertEnter',
		config = function()
			local current_dir = vim.fn.getcwd()
			if string.match(current_dir, "Atcoder") == nil then
				require("copilot").setup({
					panel = {
						enabled = false
					},
					suggestion = {
						enabled = true,
						auto_trigger = true,
						keymap = {
							accept = "<C-j>",

						}
					}
				})
				vim.keymap.set("i", "<C-c><C-t>", "<cmd>Copilot toggle<cr>")
			end
		end
	}
}

return spec
