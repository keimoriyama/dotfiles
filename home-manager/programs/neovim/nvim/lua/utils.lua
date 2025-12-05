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

-- local ok, extui = pcall(require, "vim._extui")
-- if ok then
-- 	extui.enable({
-- 		enable = true, -- extuiを有効化
-- 		msg = {
-- 			pos = "cmd", -- 'box'か'cmd'だがcmdheight=0だとどっちでも良い？（記事後述）
-- 			box = {
-- 				timeout = 5000, -- boxメッセージの表示時間 ミリ秒
-- 			},
-- 		},
-- 	})
-- end
vim.keymap.set("n", "ZR", function()
	-- force restart if count given
	if vim.v.count > 0 then
		vim.cmd("restart")
		return
	end

	-- cleanup session-unfriendly buffers (e.g., terminal)
	local bufname = vim.api.nvim_list_bufs()
	for _, buf in ipairs(bufname) do
		if vim.bo[buf].buftype == "terminal" then
			vim.api.nvim_buf_delete(buf, { force = true })
		end
	end

	-- save session and restart
	require("mini.sessions").setup()
	require("mini.sessions").write("ZR")
	vim.cmd(
		[[restart +xa lua (function() require("mini.sessions").setup(); require("mini.sessions").read("ZR") end)()]]
	)
end)
