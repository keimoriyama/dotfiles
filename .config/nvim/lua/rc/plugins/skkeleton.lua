local M = {}

function M.setup()
	vim.fn["skkeleton#config"]({
		globalJisyo = "~/.local/skkeleton/SKK-JISYO.L",
		debug = false,
	})
	vim.keymap.set({ "i", "c", "t" }, "<C-j>", "<Plug>(skkeleton-toggle)", { noremap = true })
end

return M
