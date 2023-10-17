local function python_dap_config()
	local dap = require("dap")
	dap.adapters.python = function(cb, config)
		if config.request == 'attach' then
			---@diagnostic disable-next-line: undefined-field
			local port = (config.connect or config).port
			---@diagnostic disable-next-line: undefined-field
			local host = (config.connect or config).host or '127.0.0.1'
			cb({
				type = 'server',
				port = assert(port, '`connect.port` is required for a python `attach` configuration'),
				host = host,
				options = {
					source_filetype = 'python',
				},
			})
		else
			cb({
				type = 'executable',
				command = 'path/to/virtualenvs/debugpy/bin/python',
				args = { '-m', 'debugpy.adapter' },
				options = {
					source_filetype = 'python',
				},
			})
		end
	end
end


local function rust_dap_directory()
	local mason_registry = require("mason-registry")
	local codelldb_root = mason_registry.get_package("codelldb"):get_install_path() .. "/extension"
	local codelldb_path = codelldb_root .. "/adapter/codelldb"
	local liblldb_path = codelldb_root .. "/lldb/lib/liblldb.dylib"
	return codelldb_path, liblldb_path
end


---@type LazySpec
local spec = {
	{
		"mfussenegger/nvim-dap",
		dependencies = {
			"williamboman/mason.nvim",
		},
		config = function()
			python_dap_config()
			-- keymapの設定
			-- dapの起動
			vim.keymap.set('n', '<Leader>n', function() require('dap').step_over() end)
			vim.keymap.set('n', '<Leader>s', function() require('dap').step_into() end)
			vim.keymap.set('n', '<Leader>so', function() require('dap').step_out() end)
			vim.keymap.set('n', '<Leader>b', function() require('dap').toggle_breakpoint() end)
			vim.keymap.set('n', '<Leader>B', function() require('dap').set_breakpoint() end)
			vim.keymap.set('n', '<Leader>lp',
				function() require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end)
			vim.keymap.set('n', '<Leader>dr', function() require('dap').repl.open() end)
			vim.keymap.set('n', '<Leader>dl', function() require('dap').run_last() end)
			vim.keymap.set('n', "<Leader>ed", function() require('dap').close() end)
			vim.keymap.set({ 'n', 'v' }, '<Leader>dh', function()
				require('dap.ui.widgets').hover()
			end)
			vim.keymap.set({ 'n', 'v' }, '<Leader>dp', function()
				require('dap.ui.widgets').preview()
			end)
			vim.keymap.set('n', '<Leader>df', function()
				local widgets = require('dap.ui.widgets')
				widgets.centered_float(widgets.frames)
			end)
			vim.keymap.set('n', '<Leader>ds', function()
				local widgets = require('dap.ui.widgets')
				widgets.centered_float(widgets.scopes)
			end)
		end
	},
	{
		"jay-babu/mason-nvim-dap.nvim",
		config = function()
			require('mason-nvim-dap').setup({
				ensure_installed = { "lldb" }
			}
			)
		end
	},
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
		dependencies = {
			"williamboman/mason.nvim",
			"mfussenegger/nvim-dap",
			"neovim/nvim-lspconfig",
			'nvim-lua/plenary.nvim',
		},
		-- ft = { 'rust' },
		config = function()
			-- Update this path
			local rt = require("rust-tools")
			local codelldb_path, liblldb_path = rust_dap_directory()
			local opts = {
				server = {
					on_attach = function(_, bufnr)
						-- Hover actions
						vim.keymap.set("n", "<C-a>", rt.hover_actions.hover_actions, { buffer = bufnr })

						-- Code action groups
						vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
						vim.keymap.set('n', '<Leader>st', function()
							rt.cached_commands.execute_last_debuggable()
						end, { buffer = bufnr })
					end,
				},
				hover_actions = {
					auto_focus = true,
				},
				dap = {
					adapter = require('rust-tools.dap').get_codelldb_adapter(
						codelldb_path, liblldb_path)
				}
			}
			-- Normal setup
			rt.setup(opts)
		end
	}
}

return spec
