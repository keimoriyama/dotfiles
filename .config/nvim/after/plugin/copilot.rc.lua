-- vim.g.copilot_no_tab_map = true
-- vim.cmd([[imap <silent><script><expr> <C-J> copilot#Accept("\<CR>") ]])
require("copilot").setup({
	filetypes = {
		markdown = false, -- overrides default
		terraform = false, -- disallow specific filetype
		sh = function()
			if string.match(vim.fs.basename(vim.api.nvim_buf_get_name(0)), "^%.env.*") then
				-- disable for .env files
				return false
			end
			return true
		end,
	},
})
