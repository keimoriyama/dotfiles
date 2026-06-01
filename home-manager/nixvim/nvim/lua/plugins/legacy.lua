local function source_vimscript(path)
	vim.cmd("source " .. vim.fn.fnameescape(path))
end

source_vimscript(vim.fs.joinpath(vim.fn.stdpath("config"), "vim/vimtex.vim"))

vim.cmd.colorscheme("catppuccin-latte")

require("nvim-lastplace").setup()
require("hlchunk").setup({
	chunk = { enable = true },
	indent = { enable = true },
	line_num = { enable = true },
	blank = { enable = true },
})
require("treesitter-context").setup({
	enable = true,
	min_window_height = 20,
})
require("skkeleton_indicator").setup()
require("nvim_context_vt").setup({ min_rows = 5 })
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

vim.keymap.set({ "n", "x" }, "<leader>gf", "<Plug>(gf-improved-gf)", { remap = true })
vim.keymap.set({ "n", "x" }, "<leader>gF", "<Plug>(gf-improved-gF)", { remap = true })

vim.keymap.set({ "i", "n" }, "<C-d>", "<Plug>(dmacro-play-macro)<CR>", { remap = true })

vim.keymap.set("i", "<C-g>", "copilot#Accept()", { expr = true, silent = true })

vim.api.nvim_create_autocmd("FileType", {
	pattern = { "toml", "markdown" },
	callback = function()
		vim.keymap.set("n", "<C-p>", function()
			require("nvim-treesitter-clipping").clip()
			vim.keymap.set("n", "Q", "<Cmd>w<CR><Cmd>ParteditEnd<CR>", { buffer = true })
		end, { buffer = true })
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = { "gin-diff", "gin-log", "gin-status" },
	callback = function()
		local keymap = vim.keymap.set
		local opts = { buffer = true, noremap = true }
		keymap("n", "c", "<Cmd>Gin commit<Cr>", opts)
		keymap("n", "s", "<Cmd>GinStatus<Cr>", opts)
		keymap("n", "L", "<Cmd>GinLog --graph --oneline<Cr>", opts)
		keymap("n", "d", "<Cmd>GinDiff --cached<Cr>", opts)
		keymap("n", "q", "<Cmd>bdelete<Cr>", opts)
		keymap("n", "p", [[<Cmd>lua vim.notify("Gin push")<Cr><Cmd>Gin push<Cr>]], opts)
		keymap("n", "P", [[<Cmd>lua vim.notify("Gin pull")<Cr><Cmd>Gin pull<Cr>]], opts)
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "gin-status",
	callback = function()
		local keymap = vim.keymap.set
		local opts = { buffer = true, noremap = true }
		keymap("n", "h", "<Plug>(gin-action-stage)", opts)
		keymap("n", "a", "<Plug>(gin-action-add)", opts)
		keymap("n", "l", "<Plug>(gin-action-unstage)", opts)
	end,
})

vim.keymap.set("n", "<C-a>", function()
	require("dial.map").manipulate("increment", "normal")
end)
vim.keymap.set("n", "<C-x>", function()
	require("dial.map").manipulate("decrement", "normal")
end)
vim.keymap.set("n", "g<C-a>", function()
	require("dial.map").manipulate("increment", "gnormal")
end)
vim.keymap.set("n", "g<C-x>", function()
	require("dial.map").manipulate("decrement", "gnormal")
end)
vim.keymap.set("x", "<C-a>", function()
	require("dial.map").manipulate("increment", "visual")
end)
vim.keymap.set("x", "<C-x>", function()
	require("dial.map").manipulate("decrement", "visual")
end)
vim.keymap.set("x", "g<C-a>", function()
	require("dial.map").manipulate("increment", "gvisual")
end)
vim.keymap.set("x", "g<C-x>", function()
	require("dial.map").manipulate("decrement", "gvisual")
end)

vim.keymap.set("n", "w", "<Plug>(smartword-w)", { remap = true })
vim.keymap.set("n", "b", "<Plug>(smartword-b)", { remap = true })
vim.keymap.set("n", "e", "<Plug>(smartword-e)", { remap = true })

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
