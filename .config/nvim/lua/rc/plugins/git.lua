local spec = {
	-- git
	{
		"dinhhuy258/git.nvim",
		config = function()
			local status, git = pcall(require, "git")
			if not status then
				return
			end
			require('git').setup({
				default_mappings = true
			})
		end
	},
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
		end,
	},
}
return spec
