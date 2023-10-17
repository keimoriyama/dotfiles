local function rust_dap_config()
	local dap = require("dap")

	dap.adapters.codelldb = {
		type = 'server',
		command = 'codelldb',
		port = 45635,
	}
	dap.configurations.codelldb = {
		type = 'server',
		host = '127.0.0.1',
		port = 45635,
	}
	dap.configurations.rust = {
		{
			name = 'Rust Debug',
			type = 'codelldb',
			request = 'launch',
			program = function()
				return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
			end,
			cwd = '${workspaceFolder}',
			stopOnEntry = false,
			args = {}
		}
	}
end

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
---@type LazySpec
local spec = {
	{
		"mfussenegger/nvim-dap",
		dependencies = {
			"williamboman/mason.nvim"
		},
		config = function()
			rust_dap_config()
			python_dap_config()
			-- keymapの設定
			-- TODO:見直す
			-- dapの起動
			vim.keymap.set('n', '<Leader>n', function() require('dap').continue() end)
			vim.keymap.set('n', '<Leader>so', function() require('dap').step_over() end)
			vim.keymap.set('n', '<Leader>si', function() require('dap').step_into() end)
			vim.keymap.set('n', '<Leader>so', function() require('dap').step_out() end)
			vim.keymap.set('n', '<Leader>b', function() require('dap').toggle_breakpoint() end)
			vim.keymap.set('n', '<Leader>B', function() require('dap').set_breakpoint() end)
			vim.keymap.set('n', '<Leader>lp',
				function() require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end)
			vim.keymap.set('n', '<Leader>dr', function() require('dap').repl.open() end)
			vim.keymap.set('n', '<Leader>dl', function() require('dap').run_last() end)
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
}

return spec
