-- lua_source {{{
local treesitter = require("nvim-treesitter")
treesitter.setup({
	install_dir = vim.fs.joinpath(vim.fn.stdpath("data"), "site"),
})

local installed_parser = { "nix", "c", "lua", "vim", "vimdoc", "python", "toml", "json", "latex", "typst" }
treesitter.install(installed_parser, {})

vim.api.nvim_create_autocmd({ "FileType" }, {
	pattern = installed_parser,
	callback = function()
		-- syntax highlighting, provided by Neovim
		vim.treesitter.start()
		-- folds, provided by Neovim
		vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
		-- indentation, provided by nvim-treesitter
		vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
		vim.o.foldmethod = "expr"
	end,
})
-- }}}
