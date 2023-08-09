local M = {}
function M.setup()
	local status, autopairs = pcall(require, "nvim-autopairs")
	if not status then
		return
	end
	autopairs.setup({
		disable_filetype = { "TelescopePrompt", "vim" },
	})
end

return M
