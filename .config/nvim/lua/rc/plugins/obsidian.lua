local M = {}

function M.setup()
	local status, obsidian = pcall(require, "obsidian")
	if not status then
		return
	end
	obsidian.setup({
		dir = "~/Documents/Notes",
		daily_notes = {
			-- Optional, if you keep daily notes in a separate directory.
			folder = "Notes/daily",
			-- Optional, if you want to change the date format for daily notes.
			date_format = "%Y-%m-%d"
		},
		completion = {
			-- If using nvim-cmp, otherwise set to false
			nvim_cmp = true
		},
		mappings = {
			["ol"] = require("obsidian.mapping").gf_passthrough(),
		},
	})
end

return M
