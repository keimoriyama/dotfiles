local spec = {
		{
		"akinsho/toggleterm.nvim",
		keys = { { "<C-t>", ":ToggleTerm<CR>" } },
		config = function()
			local status, toggleterm = pcall(require, "toggleterm")

			if not status then
				return
			end

			toggleterm.setup()

			local option = { noremap = true, silent = true }
			vim.api.nvim_set_keymap({ "n", "t" }, "<C-t>", "<cmd>ToggleTerm<CR>", option)
		end,
	},
}

return spec
