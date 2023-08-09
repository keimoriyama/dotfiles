local M = {}

function M.setup()
	local status, gitsings = pcall(require, "gitsings")
	if not status then
		return
	end

	gitsings.setup()
end
return M
