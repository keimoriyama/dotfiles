-- Clone 'mini.nvim' manually in a way that it gets managed by 'mini.deps'
local path_package = vim.fn.stdpath("data") .. "/site/"
-- print(path_package)
local mini_path = path_package .. "pack/deps/start/mini.nvim"
if not vim.loop.fs_stat(mini_path) then
	vim.cmd('echo "Installing `mini.nvim`" | redraw')
	local clone_cmd = {
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/echasnovski/mini.nvim",
		mini_path,
	}
	vim.fn.system(clone_cmd)
	vim.cmd("packadd mini.nvim | helptags ALL")
	vim.cmd('echo "Installed `mini.nvim`" | redraw')
end

-- Set up 'mini.deps' (customize to your liking)
require("mini.deps").setup({ path = { package = path_package } })
local now, later, add = MiniDeps.now, MiniDeps.later, MiniDeps.add

-- Safely execute immediately
now(function()
	vim.o.termguicolors = true
	vim.cmd("colorscheme randomhue")
	require("mini.notify").setup()
	vim.api.nvim_create_user_command("NotifyLog", function()
		-- local logs = vim.inspect(MiniNotify.get_all())
		local logs = MiniNotify.get_all()
		for i, log in ipairs(logs) do
			print(log.msg)
		end
	end, {})
	vim.notify = require("mini.notify").make_notify()
	require("mini.tabline").setup()
	require("mini.statusline").setup({
		set_vim_settings = false,
	})
	require("mini.starter").setup()
	require("mini.surround").setup({
		mappings = {
			add = "ya", -- Add surrounding in Normal and Visual modes
			delete = "yd", -- Delete surrounding
			find = "yf", -- Find surrounding (to the right)
			find_left = "yF", -- Find surrounding (to the left)
			highlight = "yh", -- Highlight surrounding
			replace = "yr", -- Replace surrounding
			update_n_lines = "yn", -- Update `n_lines`
		},
	})
	require("mini.indentscope").setup({
		draw = {
			delay = 0,
		},
	})
	require("mini.pairs").setup()
	require("mini.cursorword").setup()
	require("mini.bracketed").setup()
	require("mini.icons").setup()
	MiniIcons.mock_nvim_web_devicons()

	require("mini.completion").setup({
		lsp_completion = {
			source_func = "completefunc",
			auto_setup = true,
		},
		window = {
			info = { border = "double" },
			signature = { border = "double" },
		},
	})
	-- utils
	add({ source = "nvim-lua/plenary.nvim" })
	add({ source = "ethanholz/nvim-lastplace" })
	require("nvim-lastplace").setup()
	add("shortcuts/no-neck-pain.nvim")
	local width = vim.fn.winwidth(0)
	if width >= 100 then
		require("no-neck-pain").enable()
	end
	vim.api.nvim_create_autocmd({ "VimResized", "WinResized" }, {
		callback = function()
			width = vim.fn.winwidth(0)
			if width >= 100 then
				require("no-neck-pain").enable()
			end
		end,
	})
end)

now(function()
	add({
		source = "williamboman/mason.nvim",
		depends = { "williamboman/mason-lspconfig.nvim", "ray-x/lsp_signature.nvim" },
	})
	---@diagnostic disable: redefined-local
	local status, mason = pcall(require, "mason")
	if not status then
		return
	end

	mason.setup({
		ui = {
			icons = {
				package_installed = "✓",
				package_pending = "➜",
				package_uninstalled = "✗",
			},
		},
	})

	-- add lsp
	local servers = {
		"lua_ls",
		"html",
		"quick_lint_js",
		"jsonls",
		"pyright",
	}

	local status, mason_lspconfig = pcall(require, "mason-lspconfig")
	if not status then
		return
	end
	mason_lspconfig.setup({ ensure_installed = servers })
end)

now(function()
	add({ source = "neovim/nvim-lspconfig" })
	local ok, wf = pcall(require, "vim.lsp._watchfiles")
	if ok then
		wf._watchfunc = function()
			return function() end
		end
	end

	local opts = { noremap = true, silent = true }

	local status, nvim_lsp = pcall(require, "lspconfig")
	if not status then
		return
	end
	nvim_lsp.lua_ls.setup({
		settings = {
			Lua = {
				diagnostics = {
					globals = { "vim", "hs", "wez" },
				},
			},
		},
	})

	nvim_lsp.pyright.setup({
		settings = {
			python = {
				venvPath = ".",
				pythonPath = "./.venv/bin/python",
				analysis = {
					extraPaths = { "." },
				},
			},
		},
	})

	nvim_lsp.denols.setup({
		root_dir = nvim_lsp.util.root_pattern("deno.json"),
		init_options = {
			lint = true,
			unstable = false,
			suggest = {
				imports = {
					hosts = {
						["https://deno.land"] = true,
						["https://cdn.nest.land"] = true,
						["https://crux.land"] = true,
					},
				},
			},
		},
	})

	vim.lsp.handlers["textDocument/publishDiagnostics"] =
		vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, { virtual_text = true })
	-- vim.diagnostics.config({ severity_sort = true })
	-- LSP handlers
	vim.api.nvim_create_autocmd("LspAttach", {
		group = vim.api.nvim_create_augroup("UserLspConfig", {}),
		callback = function(ev)
			-- Enable completion triggered by <c-x><c-o>
			vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
			local opt = { noremap = true, silent = true, buffer = ev.buf }
			-- Mappings.
			-- See `:help vim.lsp.*` for documentation on any of the below functions
			vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opt)
			vim.keymap.set("n", "gd", vim.lsp.buf.definition, opt)
			vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opt)
			vim.keymap.set("n", "gr", vim.lsp.buf.references, opt)
			vim.keymap.set("n", "H", vim.lsp.buf.hover, opt)
			vim.keymap.set("n", "K", vim.lsp.buf.type_definition, opt)
			vim.keymap.set("n", "<Leader>D", vim.lsp.buf.type_definition, opt)
			vim.keymap.set("n", "<Leader>rn", vim.lsp.buf.rename, opt)
			vim.keymap.set("n", "<Leader>bf", "<cmd>lua vim.lsp.buf.format({async=true})<CR>", opt)
			vim.keymap.set("n", "<Leader>ic", vim.lsp.buf.incoming_calls, opt)
			vim.keymap.set("n", "[e", vim.diagnostic.goto_next, opt)
			vim.keymap.set("n", "]e", vim.diagnostic.goto_prev, opt)
			vim.keymap.set("n", "<Leader>e", vim.diagnostic.open_float, opts)
			-- Reference highlight
			local client = vim.lsp.get_client_by_id(ev.data.client_id)
			if client.server_capabilities.documentHighlightProvider then
				vim.api.nvim_command(
					"highlight LspReferenceText  cterm=underline ctermbg=8 gui=underline guibg=#104040"
				)
				vim.api.nvim_command(
					"highlight LspReferenceRead  cterm=underline ctermbg=8 gui=underline guibg=#104040"
				)
				vim.api.nvim_command(
					"highlight LspReferenceWrite cterm=underline ctermbg=8 gui=underline guibg=#104040"
				)
				vim.api.nvim_command("set updatetime=100")
				vim.api.nvim_create_augroup("lsp_document_highlight", { clear = true })
				vim.api.nvim_clear_autocmds({
					buffer = ev.buf,
					group = "lsp_document_highlight",
				})
				vim.api.nvim_create_autocmd("CursorHold", {
					callback = vim.lsp.buf.document_highlight,
					buffer = ev.buf,
					group = "lsp_document_highlight",
					desc = "Document Highlight",
				})
				vim.api.nvim_create_autocmd("CursorMoved", {
					callback = vim.lsp.buf.clear_references,
					buffer = ev.buf,
					group = "lsp_document_highlight",
					desc = "Clear All the References",
				})
			end
		end,
	})
	-- https://zenn.dev/vim_jp/articles/c62b397647e3c9 エラー警告ヒントの順番を固定
	-- vim.diagnostic.config({ severity_sort = true })
end)

now(function()
	add({ source = "hrsh7th/nvim-insx" })
	require("insx.preset.standard").setup()
	local insx = require("insx")
	insx.add(
		"<C-]>",
		require("insx.recipe.fast_wrap")({
			close = '"',
		})
	)
	insx.add(
		"<C-]>",
		require("insx.recipe.fast_wrap")({
			close = "'",
		})
	)
end)

now(function()
	vim.cmd([[
	let g:vimtex_view_method = "skim"
	let g:vimtex_view_general_viewer = "skim"
	let g:vimtex_view_skim_activate = 1
	let g:vimtex_view_skim_synmc = 1
	let g:vimtex_compiler_method = "latexmk"
	let g:vimtex_view_compiler_latexmk_engines = [ "-", "-pdf" ]
	let g:latex_latexmk_options = "-pdf"
	let g:vimtex_syntax_enabled = 0
	let g:vimtex_compiler_latexmk = {
		  \ 'background': 1,
		  \ 'build_dir': '',
		  \ 'continuous': 1,
		  \ 'options': [
		  \    '-pdfdvi',
		  \    '-verbose',
		  \    '-file-line-error',
		  \    '-synctex=1',
		  \    '-interaction=nonstopmode',
		  \],
		  \}
  ]])
	add({ source = "lervag/vimtex" })
end)

now(function()
	add({
		source = "vim-skk/skkeleton",
		depends = {
			"vim-denops/denops.vim",
			"delphinus/skkeleton_indicator.nvim",
			"keimoriyama/skkeleton-azik-kanatable",
			"skk-dev/dict",
		},
	})
	vim.keymap.set("i", "<C-j>", "<Plug>(skkeleton-toggle)", { noremap = true })
	-- vim.keymap.set("i", "<C-l>", "<Plug>(skkeleton-disable)", { noremap = true })

	local path_package = vim.fn.stdpath("data") .. "/site/"
	local dictdir = path_package .. "pack/deps/opt/dict"

	local userDict = vim.fn.expand("$HOME") .. "/Documents/skk-jisyo.utf-8"

	vim.fn["skkeleton#azik#add_table"]("us")

	vim.fn["skkeleton#config"]({
		kanaTable = "azik",
	})
	vim.fn["skkeleton#register_keymap"]("henkan", "X", "")

	vim.fn["skkeleton#config"]({
		globalDictionaries = {
			vim.fs.joinpath(dictdir, "SKK-JISYO.L"),
			vim.fs.joinpath(dictdir, "SKK-JISYO.edict"),
			vim.fs.joinpath(dictdir, "SKK-JISYO.edict2"),
			vim.fs.joinpath(dictdir, "SKK-JISYO.fullname"),
			vim.fs.joinpath(dictdir, "SKK-JISYO.propernoun"),
		},
		userDictionary = userDict,
		debug = false,
	})
	require("skkeleton_indicator").setup()
end)

-- load later plugins

later(function()
	require("mini.ai").setup()
	require("mini.comment").setup()
	require("mini.files").setup({
		windows = { preview = true },
	})
	vim.keymap.set("n", "<leader>sf", "<cmd>lua MiniFiles.open()<cr>")
	require("mini.git").setup()
	git_setup()
	require("mini.diff").setup()
	require("mini.jump").setup()
	require("mini.trailspace").setup()
	require("mini.bufremove").setup()
	bufremove_setup()
	-- require("mini.map").setup()
	require("mini.extra").setup()
	-- -- telescope的なやつ
	require("mini.pick").setup()
	vim.keymap.set("n", "<leader>sf", "<Cmd>Pick explorer<Cr>", opts)
	vim.keymap.set("n", "<leader>sb", "<Cmd>Pick buffers<Cr>", opts)
	vim.keymap.set("n", "<leader>h", "<Cmd>Pick help<Cr>", opts)
	vim.keymap.set("n", "<leader>fr", "<Cmd>Pick grep<Cr>", opts)
	vim.keymap.set("n", "<leader>ff", "<Cmd>Pick files<Cr>", opts)
	vim.keymap.set("n", "<leader>gf", "<Cmd>Pick git_files<Cr>", opts)
	vim.keymap.set("n", "/", "<Cmd>Pick buf_lines<Cr>", opts)
	-- vim.keymap.set('n', [[\m]], '<Cmd>Pick visit_paths<Cr>', opts)

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
	add({ source = "nvimtools/none-ls.nvim", depends = { "nvim-lua/plenary.nvim" } })
	none_ls_config()
	add({
		source = "nvim-treesitter/nvim-treesitter",
		-- Use 'master' while monitoring updates in 'main'
		checkout = "master",
		monitor = "main",
		-- Perform action after every checkout
		hooks = {
			post_checkout = function()
				vim.cmd("TSUpdate")
			end,
		},
	})
	treesitter_setup()
	add({ source = "yioneko/nvim-yati", depends = { "nvim-treesitter/nvim-treesitter" } })
	add({ source = "HiPhish/rainbow-delimiters.nvim", depends = { "nvim-treesitter/nvim-treesitter" } })
	add({ source = "nvim-treesitter/nvim-treesitter-context", depends = { "nvim-treesitter/nvim-treesitter" } })
	require("treesitter-context").setup()
end)

function git_setup()
	vim.keymap.set({ "n", "x" }, "<Leader>gs", "<CMD>Git status<cr>", { desc = "Show at cursor" })
	vim.keymap.set({ "n", "x" }, "<Leader>gc", "<CMD>Git commit<CR>", { desc = "Show at cursor" })
	vim.keymap.set({ "n", "x" }, "<Leader>ga", "<CMD>Git add .<CR>", { desc = "Show at cursor" })
end

function bufremove_setup()
	local function delete_buf()
		local bufnr = vim.api.nvim_get_current_buf()
		MiniBufremove.delete(bufnr)
	end
	vim.keymap.set("n", "<leader>bd", function()
		delete_buf()
	end)
end

function none_ls_config()
	local status, null_ls = pcall(require, "null-ls")
	if not status then
		return
	end

	local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

	local on_attach = function(client, bufnr)
		-- you can reuse a shared lspconfig on_attach callback here
		if client.supports_method("textDocument/formatting") then
			vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
			vim.api.nvim_create_autocmd("BufWritePre", {
				group = augroup,
				buffer = bufnr,
				callback = function()
					vim.lsp.buf.format({ bufnr = bufnr })
				end,
			})
		end
	end

	null_ls.setup({
		debug = false,
		sources = {
			null_ls.builtins.formatting.stylua,
			null_ls.builtins.formatting.black,
			null_ls.builtins.formatting.isort,
		},
		on_attach = on_attach,
	})
end

function treesitter_setup()
	local status, ts = pcall(require, "nvim-treesitter.configs")
	if not status then
		return
	end

	ts.setup({
		highlight = {
			enable = true,
			additional_vim_regex_highlighting = { "markdown" },
			disable = function(lang, buf)
				local max_filesize = 100 * 1024 -- 100 KB
				local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
				if ok and stats and stats.size > max_filesize then
					vim.api.nvim_out_write("Warning: File size exceeds 100KB. Disabling Treesitter highlighting.\n")
					return true
				end
			end,
		},
		indent = { enable = false, disable = { "python" } },
		ensure_installed = {
			"toml",
			"json",
			"yaml",
			"lua",
			"python",
			"markdown",
			"markdown_inline",
			"vim",
			"dockerfile",
			"make",
		},
		autotag = { enable = true },
		yati = {
			enable = true,
			indent = { enable = false },
		},
	})

	local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
	parser_config.tsx.filetype_to_parsername = { "javascript", "typescript.tsx" }
end
