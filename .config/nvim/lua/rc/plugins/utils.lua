local M = {}

local add, later, now = MiniDeps.add, MiniDeps.later, MiniDeps.now

function M.setup()
	now(function()
		add({ source = "ethanholz/nvim-lastplace" })
		require("nvim-lastplace").setup()
		vim.api.nvim_feedkeys("zz", "n", false)
		add("shortcuts/no-neck-pain.nvim")
		local width = vim.fn.winwidth(0)
		if width >= 100 then
			require("no-neck-pain").enable()
		end
	end)
	later(function()
		-- utils
		add({ source = "nvim-lua/plenary.nvim" })
		-- terminal
		add({ source = "uga-rosa/ugaterm.nvim" })
		vim.keymap.set({ "n", "t" }, "<C-t>", "<cmd>UgatermOpen -toggle<cr>", { noremap = true, silent = true })
		-- sonic template
		vim.g.sonictemplate_key = 0
		vim.g.sonictemplate_intelligent_key = 0
		vim.g.sonictemplate_postfix_key = 0
		vim.g.sonictemplate_vim_template_dir = "~/.dotfiles/.config/nvim/template"
		add({ source = "mattn/vim-sonictemplate" })
		-- smart word
		add("kana/vim-smartword")
		vim.keymap.set("n", "w", "<Plug>(smartword-w)zz")
		vim.keymap.set("n", "b", "<Plug>(smartword-b)zz")
		vim.keymap.set("n", "e", "<Plug>(smartword-e)zz")

		add("chrisbra/Recover.vim")
		add({ source = "folke/noice.nvim", depends = { "MunifTanjim/nui.nvim" } })
		noice_setup()
	end)
end

function noice_setup()
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
		notify = { enabled = false },
	})
end

return M
