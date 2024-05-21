local M = {}

local add, later = MiniDeps.add, MiniDeps.later

function M.setup()
	later(function()
		add({
			source = "epwalsh/obsidian.nvim",
			depends = {
				-- Required.
				"nvim-lua/plenary.nvim",
				"telescope.nvim",
				"hrsh7th/nvim-cmp",
			},
		})

		-- local base_dir = vim.fn.expand("$HOME/Program/vim_plugins/")
		-- vim.opt.rtp:append(base_dir .. "obsidian.nvim")
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
		local opt = { noremap = false, expr = true }
		vim.keymap.set("n", "<Leader>gf", function()
			if require("obsidian").util.cursor_on_markdown_link() then
				return "<CMD>ObsidianFollowLink<CR>"
			else
				print("Cursor is not on a markdown link")
			end
		end, opts)
		vim.keymap.set("n", "<leader>nn", "<CMD>ObsidianToday<CR>", opts)
		vim.keymap.set("n", "<leader>fn", "<CMD>ObsidianSearch<CR>", opts)
	end)
end

return M
