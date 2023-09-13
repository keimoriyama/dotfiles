local spec = {
	{ "mfussenegger/nvim-dap" },
	{ "jay-babu/mason-nvim-dap.nvim" },
	{
		"rcarriga/nvim-dap-ui",
		config = function()
			require("dapui").setup()
			local dap, dapui = require("dap"), require("dapui")
			dap.listeners.after.event_initialized["dapui_config"] = function()
				dapui.open()
			end
			dap.listeners.before.event_terminated["dapui_config"] = function()
				dapui.close()
			end
			dap.listeners.before.event_exited["dapui_config"] = function()
				dapui.close()
			end
		end
	},
	{
		"simrat39/rust-tools.nvim",
		ft = { 'rust' },
		config = function()
			local rt = require("rust-tools")
			rt.setup({
				server = {
					on_attach = function(_, bufnr)
						-- Hover actions
						local opt = { nnopremap = true, silent = true, buffer = bufnr }
						vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, opt)
						-- Code action groups
						vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, opt)
					end,
				},
			})
		end
	},
	{
		"mfussenegger/nvim-dap-python",
		ft = { 'python' },
		config = function()
			local dap_python = require('dap-python')
			dap_python.setup("~/.virtualenvs/debugpy/bin/python")
			dap_python.test_runner = 'pytest'

			local opt = { noremap = true, silent = true }
			vim.keymap.set("n", "<leader>dn", "<cmd>lua require('dap-python').test_method()<cr>", opt)
			vim.keymap.set("n", "<leader>df", "<cmd>lua require('dap-python').test_class()<cr>", opt)
			vim.keymap.set("n", "<leader>ds", "<ESC><cmd>lua require('dap-python').debug_selection()<CR>", opt)
		end
	}
}

return spec
