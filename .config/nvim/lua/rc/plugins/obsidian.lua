---@type LazySpec
local spec = {
	-- {
	-- 	"epwalsh/obsidian.nvim",
	-- 	config = function()
	-- 		local status, obsidian = pcall(require, "obsidian")
	-- 		if not status then
	-- 			return
	-- 		end
	-- 		obsidian.setup({
	-- 			dir = "~/Documents/Notes",
	-- 			daily_notes = {
	-- 				-- Optional, if you keep daily notes in a separate directory.
	-- 				folder = "daily",
	-- 				-- Optional, if you want to change the date format for daily notes.
	-- 				date_format = "%Y-%m-%d",
	-- 			},
	-- 			completion = {
	-- 				-- If using nvim-cmp, otherwise set to false
	-- 				nvim_cmp = false,
	-- 			},
	-- 		})
	-- 		local opts = { noremap = true, silent = true }
	-- 		vim.keymap.set("n", "<leader>nn", "<cmd>ObsidianToday<cr>", opts)
	-- 		-- vim.keymap.set("n", "gf", "<cmd>ObsidianFollowLink<CR>", opts)
	-- 		vim.keymap.set("n", "<leader>os", "<cmd>ObsidianSearch<cr>", opts)
	-- 	end,
	-- },
	{
		-- "keimoriyama/dps_obdisian",
		dir = "~/Program/vim_plugins/dps_obsidian",
		dependencies = {
			'vim-denops/denops.vim',
		},
		init = function()
			vim.g.dps_obsidian_base_dir = "~/Documents/Notes"
			vim.g.dps_obsidian_daily_note_dir = "daily"
		end,
		config = function()
			local opts = { noremap = true, silent = true }
			vim.keymap.set("n", "<leader>nn", "<cmd>DpsObsidianToday<cr>", opts)
			vim.keymap.set("n", "gf", "<cmd>DpsObsidianFollowLink<CR>", opts)
		end,
	}
}
-- return {}
return spec
