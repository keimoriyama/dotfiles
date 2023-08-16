local M = {}

local prev_buffer_config

function M.setup()
	vim.fn["skkeleton#config"]({
		globalJisyo = "~/.local/skkeleton/SKK-JISYO.L",
		debug = false,
		showCandidatesCount = 1
	})
	vim.keymap.set({ "i", "c", "t" }, "<C-k>", "<Plug>(skkeleton-toggle)", { noremap = true })
end

return M
