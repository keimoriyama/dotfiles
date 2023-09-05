local spec = {
	{
		"numToStr/Comment.nvim",
		config = function()
			local status, comment = pcall(require, "Comment")
			if not status then
				return
			end

			comment.setup({
				---LHS of toggle mappings in NORMAL mode
				---@type table
				toggler = {
					---Line-comment toggle keymap
					line = "col",
					---Block-comment toggle keymap
					block = "cob",
				},
			})
		end,
	},
}
return spec
