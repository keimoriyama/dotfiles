---@type LazySpec
local spec = {
	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		config = function()
			local status, autopairs = pcall(require, "nvim-autopairs")
			if not status then
				return
			end
			autopairs.setup({
				disable_filetype = { "TelescopePrompt", "vim" },
			})
		end,
	},
}
return spec
