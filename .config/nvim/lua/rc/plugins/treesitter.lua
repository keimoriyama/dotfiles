local spec = {
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			local status, ts = pcall(require, "nvim-treesitter.configs")
			if not status then
				return
			end

			ts.setup({
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
					"markdown",
					"markdown_inline",
					"latex",
				},
				autotag = { enable = true },
				yati = { enable = true, indent = { enable = false } },
				update_strategy = "lockfile",
			})

			local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
			parser_config.tsx.filetype_to_parsername = { "javascript", "typescript.tsx" }
		end,
	},
		{
		"yioneko/nvim-yati",
		dependencies = "nvim-treesitter/nvim-treesitter"
	},
}
return spec
