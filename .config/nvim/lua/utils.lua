local path = vim.fn.expand('%:p')

if (vim.fn.isdirectory(path) == 1) then
	vim.api.nvim_create_autocmd({ "BufEnter" },
		{
			once = true,
			callback = function()
				-- Dduの設定を読み込む
				vim.cmd([[
					:Ddu file -name=filer<cr>
				]])
			end
		})
	return
end
