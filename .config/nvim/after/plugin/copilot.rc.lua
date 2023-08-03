-- vim.cmd([[imap <silent><script><expr> <C-j> copilot#Accept("\<CR>") ]])
-- vim.g.copilot_no_maps = true
require("copilot").setup({
	suggestion = { enabled = false },
	panel = { enabled = false },
})
