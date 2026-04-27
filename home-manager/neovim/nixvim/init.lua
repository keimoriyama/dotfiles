vim.g.mapleader = ";"

vim.opt.fileencodings = "utf-8,iso-2022-jp,euc-jp,sjis"
vim.opt.relativenumber = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.laststatus = 0
vim.opt.statusline = "─"
vim.opt.wildmenu = false
vim.opt.modeline = false
vim.opt.autoindent = true
vim.opt.autoread = true
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.writebackup = false
vim.cmd("filetype plugin indent on")
vim.cmd("syntax enable")
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.modifiable = true
vim.opt.clipboard = vim.fn.has("mac") == 1 and "unnamedplus,unnamed" or "unnamedplus"
vim.opt.splitright = true
vim.opt.cmdheight = 0
vim.opt.scrolloff = 0
vim.opt.cursorline = true
vim.opt.cursorcolumn = true
vim.opt.mouse = ""

local opts = { noremap = true, silent = true }

local function has_plug_mapping(name, mode)
	return vim.fn.maparg(name, mode or "n") ~= ""
end

local function exit_buffer()
	if vim.api.nvim_buf_get_name(0) == "" then
		vim.api.nvim_command("q")
	else
		vim.api.nvim_command("wq")
	end
end

vim.api.nvim_set_keymap("n", "<Esc><Esc>", ":<C-u>set nohlsearch<Return>", opts)
vim.api.nvim_set_keymap("n", "<Leader>w", ":w<CR>", opts)
vim.keymap.set("n", "<Leader>q", function()
	exit_buffer()
end, opts)
vim.api.nvim_set_keymap("n", "<Leader>Q", ":q!<CR>", opts)
vim.api.nvim_set_keymap("n", "+", "<C-a>", opts)
vim.api.nvim_set_keymap("n", "-", "<C-x>", opts)
vim.api.nvim_set_keymap("v", "<Leader>cw", "g<C-G>", opts)
vim.api.nvim_set_keymap("n", "<C-x>2", "<C-w>s", opts)
vim.api.nvim_set_keymap("n", "<C-x>3", "<C-w>v", opts)
vim.api.nvim_set_keymap("t", "<Esc>", "<C-\\><C-n>", opts)

vim.api.nvim_create_autocmd("BufLeave", {
	callback = function()
		if vim.bo.modified then
			vim.cmd("update")
		end
	end,
})

local cursor_palette = {
	normal = "#dc8a78",
	insert = "#40a02b",
	visual = "#df8e1d",
	replace = "#d20f39",
	command = "#1e66f5",
	terminal = "#8839ef",
}

local function cursor_mode(mode)
	if mode:match("^i") then
		return "insert"
	elseif mode == "V" or mode == "\22" or mode:match("^v") or mode:match("^s") or mode == "S" then
		return "visual"
	elseif mode:match("^R") or mode:match("^r") then
		return "replace"
	elseif mode:match("^c") then
		return "command"
	elseif mode:match("^t") then
		return "terminal"
	end

	return "normal"
end

local function apply_cursor_color()
	local color = cursor_palette[cursor_mode(vim.api.nvim_get_mode().mode)] or cursor_palette.normal
	vim.api.nvim_set_hl(0, "Cursor", { fg = "#eff1f5", bg = color })
	vim.api.nvim_set_hl(0, "TermCursor", { fg = "#eff1f5", bg = color })
end

vim.api.nvim_create_autocmd({ "ModeChanged", "ColorScheme", "VimEnter" }, {
	group = vim.api.nvim_create_augroup("cursor_mode_color", { clear = true }),
	callback = apply_cursor_color,
})

apply_cursor_color()

local MiniIcons = require("mini.icons")
MiniIcons.setup({})
MiniIcons.tweak_lsp_kind()
MiniIcons.mock_nvim_web_devicons()

require("mini.comment").setup()
require("mini.pairs").setup()
require("mini.surround").setup()
require("mini.ai").setup()
require("mini.jump").setup()
require("mini.diff").setup({
	view = {
		style = vim.go.number and "number" or "sign",
	},
})
require("mini.git").setup()
require("mini.tabline").setup()
require("mini.bracketed").setup()

local miniclue = require("mini.clue")
miniclue.setup({
	triggers = {
		{ mode = { "n", "x" }, keys = "<Leader>" },
		{ mode = "n", keys = "[" },
		{ mode = "n", keys = "]" },
		{ mode = "i", keys = "<C-x>" },
		{ mode = { "n", "x" }, keys = "g" },
		{ mode = { "n", "x" }, keys = "'" },
		{ mode = { "n", "x" }, keys = "`" },
		{ mode = { "n", "x" }, keys = '"' },
		{ mode = { "i", "c" }, keys = "<C-r>" },
		{ mode = "n", keys = "<C-w>" },
		{ mode = { "n", "x" }, keys = "z" },
	},
	clues = {
		miniclue.gen_clues.square_brackets(),
		miniclue.gen_clues.builtin_completion(),
		miniclue.gen_clues.g(),
		miniclue.gen_clues.marks(),
		miniclue.gen_clues.registers(),
		miniclue.gen_clues.windows(),
		miniclue.gen_clues.z(),
	},
})

local MiniHipatterns = require("mini.hipatterns")
MiniHipatterns.setup({
	highlighters = {
		fixme = { pattern = "%f[%w]()FIXME()%f[%W]", group = "MiniHipatternsFixme" },
		hack = { pattern = "%f[%w]()HACK()%f[%W]", group = "MiniHipatternsHack" },
		todo = { pattern = "%f[%w]()TODO()%f[%W]", group = "MiniHipatternsTodo" },
		note = { pattern = "%f[%w]()NOTE()%f[%W]", group = "MiniHipatternsNote" },
		hex_color = MiniHipatterns.gen_highlighter.hex_color(),
	},
})

require("mini.completion").setup({
	lsp_completion = {
		source_func = "omnifunc",
		auto_setup = true,
	},
})

local treesitter = require("nvim-treesitter")
treesitter.setup({
	install_dir = vim.fs.joinpath(vim.fn.stdpath("data"), "site"),
})

local installed_parser = { "nix", "c", "lua", "vim", "vimdoc", "python", "toml", "json", "latex", "typst" }
treesitter.install(installed_parser, {})

vim.api.nvim_create_autocmd("FileType", {
	pattern = installed_parser,
	callback = function()
		vim.treesitter.start()
		vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
		vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
		vim.o.foldmethod = "expr"
	end,
})

local flake_path = 'builtins.getFlake "/Users/kei/dotfiles"'

vim.lsp.config("lua_ls", {
	cmd = { "lua-language-server" },
	filetypes = { "lua" },
	settings = {
		Lua = {
			diagnostics = {
				globals = { "vim", "hs", "wez" },
			},
			semantic = { enable = false },
		},
	},
})

vim.lsp.config("pyright", {
	root_markers = { ".venv" },
	settings = {
		pyright = {
			disableOrganizeImports = true,
		},
		python = {
			analysis = {
				ignore = { "*" },
			},
			venvPath = ".",
			pythonPath = "./.venv/bin/python",
		},
	},
})

vim.lsp.config("denols", {
	root_markers = { "deno.json" },
	filetypes = { "typescript" },
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

vim.lsp.config("ts_ls", {
	filetypes = { "typescript" },
	root_markers = { "package.json" },
})

vim.lsp.config("tinymist", {
	cmd = { "tinymist" },
	filetypes = { "typst" },
	single_file_support = true,
})

vim.lsp.config("docker-language-server", {
	cmd = { "docker-language-server", "start", "--stdio" },
	filetypes = { "dockerfile", "yaml.docker-compose" },
	get_language_id = function(_, ftype)
		if ftype == "yaml.docker-compose" or ftype:lower():find("ya?ml") then
			return "dockercompose"
		else
			return ftype
		end
	end,
	root_markers = {
		"Dockerfile",
		"docker-compose.yaml",
		"docker-compose.yml",
		"compose.yaml",
		"compose.yml",
		"docker-bake.json",
		"docker-bake.hcl",
		"docker-bake.override.json",
		"docker-bake.override.hcl",
	},
})

vim.lsp.config("nixd", {
	cmd = { "nixd" },
	filetypes = { "nix" },
	settings = {
		nixd = {
			formatting = {
				command = { "nixfmt-rfc-style" },
			},
			options = {
				nixpkgs = {
					expr = string.format("import (%s).inputs.nixpkgs { }", flake_path),
				},
				nixos = {
					expr = string.format("(%s).homeConfigurations.myHomeConfig.options", flake_path),
				},
				home_manager = {
					expr = string.format(
						"(%s).homeConfigurations.myHomeConfig.options.home-manager.users.type.getSubOptions []",
						flake_path
					),
				},
			},
		},
	},
})

vim.lsp.enable({
	"lua_ls",
	"ruff",
	"ty",
	"denols",
	"ts_ls",
	"rust_analyzer",
	"copilot",
	"nixd",
	"texlab",
	"tinymist",
	"docker-language-server",
	"yamlls",
	"hls",
})

vim.diagnostic.config({
	virtual_text = true,
})

vim.lsp.inline_completion.enable(true)

vim.api.nvim_create_autocmd("LspAttach", {
	group = vim.api.nvim_create_augroup("UserLspConfig", {}),
	callback = function(ev)
		local opt = { noremap = true, silent = true, buffer = ev.buf }
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
		vim.keymap.set("n", "<Leader>e", vim.diagnostic.open_float, { noremap = true, silent = true })
		vim.keymap.set("i", "<c-cr>", "<cmd>lua vim.lsp.inline_completion.get()<cr>", { silent = true })

		local client = vim.lsp.get_client_by_id(ev.data.client_id)
		if client and client.server_capabilities.documentHighlightProvider then
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

pcall(function()
	require("conform").setup({
		log_level = vim.log.levels.DEBUG,
		formatters_by_ft = {
			lua = { "stylua" },
			nix = { "alejandra" },
			python = { "ruff", "ruff_organize_imports", "ruff_format", "ruff_lint" },
			rust = { "rustfmt", lsp_format = "fallback" },
			javascript = { "prettierd", "prettier", stop_after_first = true },
		},
		format_on_save = {
			timeout_ms = 500,
			lsp_format = "fallback",
		},
	})
end)

local pick = require("mini.pick")
local extra = require("mini.extra")
local files = require("mini.files")

pick.setup({})
extra.setup({})
vim.ui.select = pick.ui_select

local function buffer_lines(query)
	if query ~= nil and query ~= "" then
		vim.schedule(function()
			if pick.is_picker_active() then
				pick.set_picker_query({ query })
			end
		end)
	end
	extra.pickers.buf_lines({ scope = "current", preserve_order = false })
end

vim.keymap.set("n", "<Leader>ff", function()
	pick.builtin.files()
end, opts)
vim.keymap.set("n", "<leader>h", function()
	pick.builtin.help()
end, opts)
vim.keymap.set("n", "<leader>fr", function()
	pick.builtin.grep_live()
end, opts)
vim.keymap.set("n", "<leader>lr", function()
	extra.pickers.lsp({ scope = "references" })
end, opts)
vim.keymap.set("n", "<leader>ld", function()
	extra.pickers.diagnostic({ scope = "current" })
end, opts)
vim.keymap.set("n", "<leader>ls", function()
	extra.pickers.lsp({ scope = "document_symbol" })
end, opts)
vim.keymap.set("n", "<leader>ic", function()
	vim.lsp.buf.incoming_calls()
end, opts)
vim.keymap.set("n", "<leader>oc", function()
	vim.lsp.buf.outgoing_calls()
end, opts)
vim.keymap.set("n", "<Leader>sb", function()
	pick.builtin.buffers()
end, opts)
vim.keymap.set("n", "/", function()
	buffer_lines()
end, opts)
vim.keymap.set("n", "*", function()
	buffer_lines(vim.fn.expand("<cword>"))
end, opts)
vim.keymap.set("n", "<leader>k", function()
	extra.pickers.keymaps()
end, opts)
vim.keymap.set("n", "<leader>dp", "<cmd>DepsUpdate<cr>", opts)
vim.keymap.set("n", "n", function()
	pick.builtin.resume()
end, opts)
vim.keymap.set("n", "<leader>e", function()
	extra.pickers.diagnostic({ scope = "current" })
end, opts)
vim.keymap.set("n", "<leader>sf", function()
	local path = vim.api.nvim_buf_get_name(0)
	if path == "" then
		path = nil
	end
	files.open(path)
end, opts)

vim.cmd.colorscheme("catppuccin-latte")

pcall(function()
	require("nvim-lastplace").setup()
end)
pcall(function()
	require("hlchunk").setup({
		chunk = { enable = true },
		indent = { enable = true },
		line_num = { enable = true },
		blank = { enable = true },
	})
end)
pcall(function()
	require("treesitter-context").setup({
		enable = true,
		min_window_height = 20,
	})
end)
pcall(function()
	require("nvim_context_vt").setup({ min_rows = 5 })
end)
pcall(function()
	require("toggleterm").setup({
		size = 100,
		open_mapping = [[<c-t>]],
		hide_numbers = true,
		shade_filetypes = {},
		shade_terminals = true,
		shading_factor = 2,
		start_in_insert = true,
		insert_mappings = true,
		persist_size = true,
		direction = "float",
		close_on_exit = true,
	})
end)

if has_plug_mapping("<Plug>(gf-improved-gf)") then
	vim.keymap.set({ "n", "x" }, "<leader>gf", "<Plug>(gf-improved-gf)", { remap = true })
	vim.keymap.set({ "n", "x" }, "<leader>gF", "<Plug>(gf-improved-gF)", { remap = true })
end

if has_plug_mapping("<Plug>(dmacro-play-macro)") then
	vim.keymap.set({ "i", "n" }, "<C-d>", "<Plug>(dmacro-play-macro)<CR>", { remap = true })
end
vim.keymap.set("i", "<C-g>", "copilot#Accept()", { expr = true, silent = true })

vim.api.nvim_create_autocmd("FileType", {
	pattern = { "toml", "markdown" },
	callback = function()
		vim.keymap.set("n", "<C-p>", function()
			local ok, clipping = pcall(require, "nvim-treesitter-clipping")
			if ok then
				clipping.clip()
			end
		end, { buffer = true })

		vim.keymap.set("n", "Q", function()
			vim.cmd("w")
			if vim.fn.exists(":ParteditEnd") == 2 then
				vim.cmd("ParteditEnd")
			end
		end, { buffer = true })
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = { "gin-diff", "gin-log", "gin-status" },
	callback = function()
		if vim.fn.exists(":Gin") ~= 2 then
			return
		end

		local keymap = vim.keymap.set
		local fopts = { buffer = true, noremap = true }
		keymap("n", "c", "<Cmd>Gin commit<Cr>", fopts)
		keymap("n", "s", "<Cmd>GinStatus<Cr>", fopts)
		keymap("n", "L", "<Cmd>GinLog --graph --oneline<Cr>", fopts)
		keymap("n", "d", "<Cmd>GinDiff --cached<Cr>", fopts)
		keymap("n", "q", "<Cmd>bdelete<Cr>", fopts)
		keymap("n", "p", [[<Cmd>lua vim.notify("Gin push")<Cr><Cmd>Gin push<Cr>]], fopts)
		keymap("n", "P", [[<Cmd>lua vim.notify("Gin pull")<Cr><Cmd>Gin pull<Cr>]], fopts)
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "gin-status",
	callback = function()
		if vim.fn.exists(":Gin") ~= 2 then
			return
		end

		local keymap = vim.keymap.set
		local fopts = { buffer = true, noremap = true }
		keymap("n", "h", "<Plug>(gin-action-stage)", fopts)
		keymap("n", "a", "<Plug>(gin-action-add)", fopts)
		keymap("n", "l", "<Plug>(gin-action-unstage)", fopts)
	end,
})

local ok_dial, dial_map = pcall(require, "dial.map")
if ok_dial then
	vim.keymap.set("n", "<C-a>", function()
		dial_map.manipulate("increment", "normal")
	end)
	vim.keymap.set("n", "<C-x>", function()
		dial_map.manipulate("decrement", "normal")
	end)
	vim.keymap.set("n", "g<C-a>", function()
		dial_map.manipulate("increment", "gnormal")
	end)
	vim.keymap.set("n", "g<C-x>", function()
		dial_map.manipulate("decrement", "gnormal")
	end)
	vim.keymap.set("x", "<C-a>", function()
		dial_map.manipulate("increment", "visual")
	end)
	vim.keymap.set("x", "<C-x>", function()
		dial_map.manipulate("decrement", "visual")
	end)
	vim.keymap.set("x", "g<C-a>", function()
		dial_map.manipulate("increment", "gvisual")
	end)
	vim.keymap.set("x", "g<C-x>", function()
		dial_map.manipulate("decrement", "gvisual")
	end)
end

if has_plug_mapping("<Plug>(smartword-w)") then
	vim.keymap.set("n", "w", "<Plug>(smartword-w)", { remap = true })
	vim.keymap.set("n", "b", "<Plug>(smartword-b)", { remap = true })
	vim.keymap.set("n", "e", "<Plug>(smartword-e)", { remap = true })
end

vim.g.expand_region_text_objects = {
	["iw"] = 0,
	["iW"] = 0,
	['i"'] = 0,
	["ia"] = 0,
	["i)"] = 1,
	["il"] = 1,
	["if"] = 1,
	["af"] = 1,
	["it"] = 1,
	["ie"] = 0,
}

vim.g.silicon_options = {
	font = "Cica",
	no_line_number = false,
	no_round_corner = true,
	no_window_controls = true,
	background_color = "#aaaaff",
	line_offset = 1,
	line_pad = 2,
	pad_horiz = 80,
	pad_vert = 100,
	shadow_blur_radius = 0,
	shadow_color = "#555555",
	shadow_offset_x = 0,
	shadow_offset_y = 0,
	tab_width = 4,
	theme = "Solarized (dark)",
}

pcall(function()
	require("incline").setup({
		render = function(props)
			local buffullname = vim.api.nvim_buf_get_name(props.buf)
			local bufname_t = vim.fn.fnamemodify(buffullname, ":t")
			local bufname = (bufname_t and bufname_t ~= "") and bufname_t or "[No Name]"
			local devicon = { " " }
			local success, nvim_web_devicons = pcall(require, "nvim-web-devicons")
			if success then
				local bufname_r = vim.fn.fnamemodify(buffullname, ":r")
				local bufname_e = vim.fn.fnamemodify(buffullname, ":e")
				local base = (bufname_r and bufname_r ~= "") and bufname_r or bufname
				local ext = (bufname_e and bufname_e ~= "") and bufname_e or vim.fn.fnamemodify(base, ":t")
				local ic, hl = nvim_web_devicons.get_icon(base, ext, { default = true })
				devicon = { ic, " ", group = hl }
			end
			local modified_icon = {}
			if vim.api.nvim_get_option_value("modified", { buf = props.buf }) then
				modified_icon = vim.tbl_extend("force", { "● " }, { guifg = "#968c81" })
			end
			local linenr = vim.api.nvim_win_get_cursor(0)[1]
			return {
				"l:" .. linenr,
				" ",
				devicon,
				" ",
				bufname,
				" ",
				modified_icon,
			}
		end,
	})
end)

vim.api.nvim_create_autocmd("FileType", {
	pattern = { "tex", "bib" },
	callback = function()
		if vim.fn.exists("*vimtex#complete#omnifunc") == 1 then
			vim.opt_local.completefunc = "vimtex#complete#omnifunc"
		end
	end,
})

local ok_skel = pcall(require, "skkeleton")
if ok_skel then
	vim.keymap.set({ "i", "c" }, "<C-j>", "<Plug>(skkeleton-enable)", { remap = true })
	vim.keymap.set({ "i", "c" }, "<C-l>", "<Plug>(skkeleton-disable)", { remap = true })

	local userDict = "~/Library/Containers/net.mtgto.inputmethod.macSKK/Data/Documents/Dictionaries/skk-jisyo.utf8"
	vim.fn["skkeleton#azik#add_table"]("us")
	vim.fn["skkeleton#register_keymap"]("henkan", "X", "")
	vim.fn["skkeleton#register_keymap"]("input", "[", "katakana")
	vim.cmd([[
    call skkeleton#register_kanatable('rom', {
            \   ',': ['，', ''],
            \   '.': ['．', ''],
            \ })
  ]])
	vim.fn["skkeleton#config"]({
		kanaTable = "azik",
		sources = { "skk_dictionary", "skk_server" },
		userDictionary = userDict,
		globalDictionaries = {
			vim.fs.joinpath(vim.fn.expand("~/.skk-dict"), "SKK-JISYO.L"),
		},
		debug = false,
	})
end

local ok_ctx_vt, context_vt = pcall(require, "nvim_context_vt")
if ok_ctx_vt then
	context_vt.setup({ min_rows = 5 })
end
