local spec = {
	{ "nvim-lua/plenary.nvim" },
	{
		"ellisonleao/gruvbox.nvim",
		lazy = false,
		config = function()
			vim.cmd([[
			set background=dark
			colorscheme gruvbox
			]])
		end,
	},
	{
		"norcalli/nvim-colorizer.lua",
		event = { "BufRead", "BufEnter" },
		config = function()
			vim.opt.termguicolors = true
			require("colorizer").setup({
				"*",
			})
		end,
	},
	{
		"ethanholz/nvim-lastplace",
		event = { "BufRead", "BufEnter" },
		config = function()
			require("nvim-lastplace").setup()
			vim.api.nvim_feedkeys("zz", "m", false)
		end,
	},
	{
		"uga-rosa/ugaterm.nvim",
		keys = { "<C-t>" },
		config = function()
			vim.keymap.set({ "n", "t" }, "<C-t>", "<cmd>UgatermOpen -toggle<cr>", { noremap = true, silent = true })
		end,
	},
	{
		"kylechui/nvim-surround",
		event = { "InsertEnter" },
		config = function()
			require("nvim-surround").setup()
		end,
	},
	{
		"mattn/vim-sonictemplate",
		event = { "BufRead", "BufEnter" },
		init = function()
			vim.g.sonictemplate_key = 0
			vim.g.sonictemplate_intelligent_key = 0
			vim.g.sonictemplate_postfix_key = 0
			vim.g.sonictemplate_vim_template_dir = "~/.dotfiles/.config/nvim/template"
		end,
	},
	{
		"kana/vim-smartword",
		event = { "BufRead", "BufEnter" },
		config = function()
			vim.keymap.set("n", "w", "<Plug>(smartword-w)zz")
			vim.keymap.set("n", "b", "<Plug>(smartword-b)zz")
			vim.keymap.set("n", "e", "<Plug>(smartword-e)zz")
		end,
	},
	{
		"chrisbra/Recover.vim",
		event = { "SwapExists" },
	},
	{
		"phaazon/hop.nvim",
		event = { "BufRead", "BufEnter" },
		config = function()
			local hop = require("hop")
			hop.setup({ keys = "etovxqpdygfblzhckisuran" })
			local directions = require("hop.hint").HintDirection
			vim.keymap.set("", "f", function()
				hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true })
			end, { noremap = true })
			vim.keymap.set("", "F", function()
				hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true })
			end, { noremap = true })
			vim.keymap.set("", "t", function()
				hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true, hint_offset = -1 })
			end, { noremap = true })
			vim.keymap.set("", "T", function()
				hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true, hint_offset = 1 })
			end, { noremap = true })
		end,
	},
	{
		"folke/noice.nvim",
		event = { "BufNewFile", "BufRead" },
		dependencies = "nui.nvim",
		config = function()
			require("noice").setup({
				lsp = {
					signature = {
						enabled = false,
						auto_open = {
							enabled = false,
							-- throttle = 0, -- Debounce lsp signature help request by 50ms
						},
					},
					override = {
						["vim.lsp.util.convert_input_to_markdown_lines"] = true,
						["vim.lsp.util.stylize_markdown"] = true,
						["cmp.entry.get_documentation"] = true,
					},
				},
				views = {
					cmdline_popup = {
						position = {
							row = "10%",
							col = "50%",
						},
						size = {
							width = 60,
							height = "auto",
						},
					},
					popupmenu = {
						relative = "editor",
						position = {
							row = "10%",
							col = "50%",
						},
						size = {
							width = 60,
							height = 10,
						},
						border = {
							style = "rounded",
							padding = { 0, 1 },
						},
						win_options = {
							winhighlight = { Normal = "Normal", FloatBorder = "DiagnosticInfo" },
						},
					},
					confirm = {
						position = {
							row = "10%",
							col = "50%",
						},
					},
				},
			})
		end,
	},
	{
		"MunifTanjim/nui.nvim",
		event = { "BufNewFile", "BufRead" },
	},
	{
		"shortcuts/no-neck-pain.nvim",
		event = { "BufNewFile", "BufRead" },
		config = function()
			local width = vim.fn.winwidth(0)
			if width >= 100 then
				require("no-neck-pain").enable()
			end
		end,
	},
	{
		"tani/dmacro.nvim",
		event = { "BufNewFile", "BufRead" },
		config = function()
			require("dmacro").setup({
				dmacro_key = "<C-p>", --  you need to set the dmacro_key
			})
		end,
	},
	{
		"echasnovski/mini.diff",
		event = { "BufNewFile", "BufRead" },
		config = function()
			require("mini.diff").setup()
		end,
	},
}

return spec
