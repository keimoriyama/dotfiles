local spec = {
	{
		"nvim-telescope/telescope.nvim",
		config = function()
			telescope_setup()
		end,
		keys = {
			{
				"<leader>ff",
				function()
					require("telescope.builtin").find_files({ hidden = true, initial_mode = "normal" })
				end,
			},
			{
				"<leader>sb",
				function()
					require("telescope.builtin").buffers()
				end,
			},
			{
				"<leader>h",
				function()
					require("telescope.builtin").help_tags()
				end,
			},
			{
				"<leader>q",
				function()
					require("telescope.builtin").quickfix()
				end,
			},
			-- set('n', ';;', function() builtin.resume() end)
			{
				"<leader>e",
				function()
					require("telescope.builtin").diagnostics()
				end,
			},
			{
				"<leader>fr",
				function()
					require("telescope.builtin").live_grep()
				end,
			},
			{
				"<leader>sf",
				function()
					require("telescope").extensions.file_browser.file_browser({
						path = "%:p:h",
						cwd = vim.fn.expand("%:p:h"),
						respect_gitignore = false,
						hidden = true,
						grouped = true,
						previewer = false,
						initial_mode = "normal",
						layout_config = { height = 40 },
					})
				end,
			},
			{
				"<leader>k",
				function()
					require("telescope.builtin").keymaps()
				end,
			},
		},
		dependencies = {
			{ "nvim-telescope/telescope-file-browser.nvim" },
			{ "nvim-telescope/telescope-ui-select.nvim" },
		},
	},
}

function telescope_setup()
	local status, telescope = pcall(require, "telescope")
	if not status then
		return
	end

	local actions = require("telescope.actions")
	-- local builtin = require("telescope.builtin")
	--
	-- local function telescope_buffer_dir() return vim.fn.expand('%:p:h') end

	local fb_actions = require("telescope").extensions.file_browser.actions

	telescope.setup({
		defaults = {
			mappings = {
				n = {
					["q"] = actions.close,
					["<C-j>"] = actions.move_selection_next,
					["<C-k>"] = actions.move_selection_previous,
				},
			},
			layout_config = {
				vertical = { width = 0.8 },
				prompt_position = "bottom",
				preview_cutoff = 1,
			},
			layout_strategy = "vertical",
		},
		extensions = {
			file_browser = {
				theme = "dropdown",
				-- disables netrw and use telescope-file-browser in its place
				hijack_netrw = true,
				initial_mode = "normal",
				mappings = {
					-- your custom insert mode mappings
					["i"] = {
						["<C-w>"] = function()
							vim.cmd("normal vbd")
						end,
					},
					["n"] = {
						-- your custom normal mode mappings
						["N"] = fb_actions.create,
						["h"] = fb_actions.goto_parent_dir,
						["/"] = function()
							vim.cmd("startinsert")
						end,
					},
				},
			},
			["ui-select"] = {
				require("telescope.themes").get_dropdown({}),
			},
		},
	})

	telescope.load_extension("file_browser")
	telescope.load_extension("ui-select")
end

return spec
