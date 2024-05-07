local M = {}

local add, later = MiniDeps.add, MiniDeps.later
function M.setup()
	later(function()
		add({
			source = "nvim-telescope/telescope.nvim",
			depends = { "nvim-telescope/telescope-ui-select.nvim" },
		})
		telescope_setup()
		vim.keymap.set("n", "<leader>ff", function()
			require("telescope.builtin").find_files({ hidden = true, initial_mode = "normal" })
		end)
		vim.keymap.set("n", "<leader>sb", function()
			require("telescope.builtin").buffers()
		end)
		vim.keymap.set("n", "<leader>h", function()
			require("telescope.builtin").help_tags()
		end)
		vim.keymap.set("n", "<leader>q", function()
			require("telescope.builtin").quickfix()
		end)
		vim.keymap.set("n", "<leader>e", function()
			require("telescope.builtin").diagnostics()
		end)
		vim.keymap.set("n", "<leader>fr", function()
			require("telescope.builtin").live_grep()
		end)
		vim.keymap.set("n", "<leader>k", function()
			require("telescope.builtin").keymaps()
		end)
	end)
end

function telescope_setup()
	local status, telescope = pcall(require, "telescope")
	if not status then
		return
	end
	local actions = require("telescope.actions")

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
	})

	telescope.load_extension("ui-select")
end

return M
