-- print(vim.inspect(vim.api.nvim_list_bufs()))
local function save_all_buffer()
	local bufnrs = vim.api.nvim_list_bufs()
	for _, bufnr in ipairs(bufnrs) do
		local changed = vim.api.nvim_buf_get_option(bufnr, "modified")
		if changed then
			vim.api.nvim_command("wq")
		end
	end
end

vim.api.nvim_create_autocmd("BufLeave", {
	callback = save_all_buffer,
})
