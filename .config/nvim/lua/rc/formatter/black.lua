local M = {}

function M.call_black()
	-- 開いているバッファ内の文字列を取得（table）
	local lines = vim.api.nvim_buf_get_lines(0, 0, -1, true)
	-- バッファ内の文字列に対して、blackを適用する
	local result = vim.system(vim.split("black -", " "), { stdin = lines }):wait()
	if result.code > 0 then
		vim.notify("Failed to format", vim.log.levels.ERROR)
		return
	end
	-- 文字列の末尾？の文字を空白に置き換える
	local data = result.stdout:gsub("%s*$", "")
	-- 今の、カーソルの位置を保持する
	local cursor = vim.api.nvim_win_get_cursor(0)
	vim.api.nvim_buf_set_lines(0, 0, -1, true, vim.split(data, "\n"))
	vim.api.nvim_win_set_cursor(0, cursor)
end

return M
