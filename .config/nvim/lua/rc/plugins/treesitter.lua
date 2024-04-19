local spec = {
{"nvim-treesitter/nvim-treesitter",
event = { 'BufNewFile', 'BufRead' },
    build = { ':TSInstall! vim', ':TSUpdate' },
config = function()
	treesitter_config()
end}
}

function treesitter_config()
require("nvim-treesitter.configs").setup({
	highlight = {
		enable = true,
		additional_vim_regex_highlighting = { "markdown" },
	},
	indent = { enable = false, disable = { "python" } },
	ensure_installed = {
		"tsx",
		"toml",
		"gitignore",
		"json",
		"yaml",
		"css",
		"html",
		"lua",
		"python",
		"cpp",
		"bash",
		"vimdoc",
		"markdown",
		"markdown_inline",
		"latex",
	},
	autotag = { enable = true },
	yati = { enable = true, indent = { enable = false } },
})

local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.tsx.filetype_to_parsername = { "javascript", "typescript.tsx" }
end
return spec
