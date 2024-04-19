---@type LazySpec
local spec = {
	{
		"mattn/vim-sonictemplate",
		init = function()
			-- sonictemplateのキーマップの無効化
			vim.g.sonictemplate_key = 0
			vim.g.sonictemplate_intelligent_key = 0
			vim.g.sonictemplate_postfix_key = 0
		end,
		config = function()
			vim.g.sonictemplate_vim_template_dir = "~/.dotfiles/template"
		end,
	},
}

return spec
