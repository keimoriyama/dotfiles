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

-- for ddc.vim and cmdline.vim
function CommandlinePre(mode)
	if vim.b["prev_buffer_config"] then
		return
	end
	vim.b["prev_buffer_config"] = vim.fn["ddc#custom#get_buffer"]()
	if mode == ":" then
		vim.fn["ddc#custom#patch_buffer"]("sourceOptions", {
			["_"] = {
				keywordPattern = "[0-9a-zA-Z_:#-]*",
			},
		})
		vim.fn["ddc#custom#set_context_buffer"](function()
			local cmdline = vim.fn.getcmdline()
			if cmdline:find("^!") then
				return {
					cmdlineSources = {
						"shell_native",
						"cmdline",
						"cmdline_history",
						"around",
					},
				}
			else
				return {}
			end
		end)
	end
end
