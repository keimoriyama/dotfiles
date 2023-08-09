local M = {}
function M.setup()
	local status, gitconflict = pcall(requre, "git-conflict")
	if not status then
		return
	end

	gitconflict.setup()
end

return M
