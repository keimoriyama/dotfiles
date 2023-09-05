local spec = {
	-- git
	{
		"lewis6991/gitsigns.nvim",
		config = function()
			local status, gitsings = pcall(require, "gitsings")
			if not status then
				return
			end

			gitsings.setup()
		end,
	},
	{
		"akinsho/git-conflict.nvim",
		version = "*",
		config = function()
			local status, gitconflict = pcall(requre, "git-conflict")
			if not status then
				return
			end

			gitconflict.setup()
		end,
	},
	{
		"airblade/vim-gitgutter",
		config = function()
			vim.g.gitgutter_map_keys = 0
		end
	},
		{
		"vim-denops/denops.vim",
		dependencies = {
			-- git
			{
				"lambdalisue/gin.vim",
				config = function()
					local opts = { noremap = true, silent = true }
					vim.keymap.set("n", "<leader>gs", ":GinStatus<CR>", opts)
					vim.keymap.set("n", "<leader>ga", ":Gin add .<CR>", opts)
					vim.keymap.set("n", "<leader>gd", ":GinDiff<CR>", opts)
					vim.keymap.set("n", "<leader>gb", ":GinBranch<CR>", opts)
					vim.keymap.set("n", "<leader>gl", ":GinLog<CR>", opts)
				end
			},
		},
	},
}
return spec
