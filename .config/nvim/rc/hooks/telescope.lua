-- lua_add {{{
local status, telescope = pcall(require, "telescope")
if not status then
	return
end
local actions = require("telescope.actions")
-- local fb_actions = require("telescope").extensions.file_browser.actions
telescope.setup({
	defaults = {
		mappings = {
			i = {
				["<C-q>"] = actions.close,
				["<C-n>"] = actions.move_selection_next,
				["<C-p>"] = actions.move_selection_previous,
			},
		},
		layout_config = {
			vertical = { width = 0.8 },
			prompt_position = "bottom",
			preview_cutoff = 1,
		},
		layout_strategy = "vertical",
	},
	-- extensions = {
	-- 	file_browser = {
	-- 		theme = "dropdown",
	-- 		-- disables netrw and use telescope-file-browser in its place
	-- 		initial_mode = "normal",
	-- 		hijack_netrw = true,
	-- 		mappings = {
	-- 			["i"] = {
	-- 				-- your custom insert mode mappings
	-- 			},
	-- 			["n"] = {
	-- 				-- your custom normal mode mappings
	-- 				["N"] = fb_actions.create,
	-- 				["h"] = fb_actions.goto_parent_dir,
	-- 				["/"] = function()
	-- 					vim.cmd("startinsert")
	-- 				end,
	-- 			},
	-- 		},
	-- 	},
	-- 	-- ["ui-select"] = {
	-- 	-- 	require("telescope.themes").get_dropdown({}),
	-- 	-- },
	-- },
})

telescope.load_extension("ui-select")
telescope.load_extension('bibtex')
-- telescope.load_extension("file_browser")
vim.keymap.set("n", "<leader>ff", function()
	require("telescope.builtin").find_files({ hidden = true })
end)
vim.keymap.set("n", "<leader>sb", function()
	require("telescope.builtin").buffers()
end)
vim.keymap.set("n", "<leader>h", function()
	require("telescope.builtin").help_tags()
end)
-- vim.keymap.set("n", "<leader>q", function()
-- 	require("telescope.builtin").quickfix()
-- end)
vim.keymap.set("n", "<leader>e", function()
	require("telescope.builtin").diagnostics()
end)
vim.keymap.set("n", "<leader>fr", function()
	require("telescope.builtin").live_grep()
end)
vim.keymap.set("n", "<leader>k", function()
	require("telescope.builtin").keymaps()
end)
vim.keymap.set("n", "<leader>sf", function()
	require("telescope").extensions.file_browser.file_browser()
end)
-- }}}
