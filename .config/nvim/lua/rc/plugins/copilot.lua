---@type LazySpec
local spec = {
	{
		"zbirenbaum/copilot.lua",
		event = "InsertEnter",
		config = function()
			require("copilot").setup({
				panel = {
					enabled = false,
				},
				suggestion = {
					enabled = true,
					auto_trigger = true,
					keymap = {
						accept = "<C-j>",
					},
				},
			})
			vim.keymap.set("i", "<C-c><C-t>", "<cmd>Copilot toggle<cr>")
		end,
	},
}

return spec
