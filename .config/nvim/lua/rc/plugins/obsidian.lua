local spec = {
	{
		"epwalsh/obsidian.nvim",
		dependencies = {
			-- Required.
			"nvim-lua/plenary.nvim",
			-- see below for full list of optional dependencies 👇
			"telescope.nvim",
			"hrsh7th/nvim-cmp",
		},
		config = function()
			vim.opt.conceallevel = 0
			local status, obsidian = pcall(require, "obsidian")
			if not status then
				return
			end

			obsidian.setup({
				dir = "~/Documents/Notes/",
				completion = {
					nvim_cmp = true,
				},
				daily_notes = { folder = "daily" },
				ui = { enable = false },
			})
		end,
		keys = {
			{
				"Leader>gf",
				function()
					if require("obsidian").util.cursor_on_markdown_link() then
						return "<CMD>ObsidianFollowLink<CR>"
					else
						print("Cursor is not on a markdown link")
					end
				end,
				desc = "ObsidianFollowLink",
				{ noremap = false, expr = true },
			},
			{ "<Leader>nn", "<CMD>ObsidianToday<CR>" },
		},
	},
}

return spec
