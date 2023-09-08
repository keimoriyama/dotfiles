local spec = {
	{
		"mattn/vim-sonictemplate",
		cmd = "Template",
		config = function()
			vim.g.sonictemplate_vim_template_dir = "~/.dotfiles/template"
			vim.g.sonictemplate_key = 0
			vim.g.sonictemplate_intelligent_key = 0
			vim.g.sonictemplate_postfix_key = 0
		end,
	},
}

return spec
