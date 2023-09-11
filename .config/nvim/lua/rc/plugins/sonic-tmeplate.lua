local spec = {
	{
		"mattn/vim-sonictemplate",
		config = function()
			vim.g.sonictemplate_vim_template_dir = "~/.dotfiles/template"
		end,
	},
}

return spec
