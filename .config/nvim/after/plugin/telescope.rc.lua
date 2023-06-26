local status, telescope = pcall(require, "telescope")
if not status then
	return
end

local actions = require("telescope.actions")
local builtin = require("telescope.builtin")

local function telescope_buffer_dir()
	return vim.fn.expand("%:p:h")
end

local fb_actions = require("telescope").extensions.file_browser.actions

telescope.setup({
	defaults = { mappings = { n = { ["q"] = actions.close } } },
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
	},
})

telescope.load_extension("file_browser")
opt = { noremap = true, silent = true }
vim.keymap.set("n", "<Leader>ff", function()
	builtin.find_files({
		hidden = true,
		initial_mode = "normal",
	})
end, opt)
vim.keymap.set("n", "<Leader>fr", function()
	builtin.live_grep()
end, opt)
vim.keymap.set("n", "<Leader>fb", function()
	builtin.buffers()
end, opt)
vim.keymap.set("n", "<Leader>h", function()
	builtin.help_tags()
end, opt)

vim.keymap.set("n", "<Leader>sf", function()
	telescope.extensions.file_browser.file_browser({
		path = "%:p:h",
		cwd = telescope_buffer_dir(),
		respect_gitignore = false,
		hidden = true,
		grouped = true,
		previewer = true,
		initial_mode = "normal",
		layout_config = { height = 40 },
	})
end, opt)

vim.keymap.set("n", "<Leader>k", function()
	builtin.keymaps()
end)
