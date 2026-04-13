-- General autocmds that don't belong to specific plugins

-- Auto-save buffers on leave
local function save_all_buffer()
	if vim.bo.modified then
		vim.cmd("update")
	end
end

vim.api.nvim_create_autocmd("BufLeave", {
	callback = save_all_buffer,
})

local cursor_palette = {
	normal = "#dc8a78",
	insert = "#40a02b",
	visual = "#df8e1d",
	replace = "#d20f39",
	command = "#1e66f5",
	terminal = "#8839ef",
}

local function cursor_mode(mode)
	if mode:match("^i") then
		return "insert"
	elseif mode == "V" or mode == "\22" or mode:match("^v") or mode:match("^s") or mode == "S" then
		return "visual"
	elseif mode:match("^R") or mode:match("^r") then
		return "replace"
	elseif mode:match("^c") then
		return "command"
	elseif mode:match("^t") then
		return "terminal"
	end

	return "normal"
end

local function apply_cursor_color()
	local mode = vim.api.nvim_get_mode().mode
	local color = cursor_palette[cursor_mode(mode)] or cursor_palette.normal

	vim.api.nvim_set_hl(0, "Cursor", { fg = "#eff1f5", bg = color })
	vim.api.nvim_set_hl(0, "TermCursor", { fg = "#eff1f5", bg = color })
end

vim.api.nvim_create_autocmd({ "ModeChanged", "ColorScheme", "VimEnter" }, {
	group = vim.api.nvim_create_augroup("cursor_mode_color", { clear = true }),
	callback = apply_cursor_color,
})

apply_cursor_color()
