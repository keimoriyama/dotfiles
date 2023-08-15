local M = {}

local prev_buffer_config

function M.setup()
	vim.fn["skkeleton#config"]({
		globalJisyo = "~/.local/skkeleton/SKK-JISYO.L",
		debug = true,
		showCandidatesCount = 1
	})
	vim.keymap.set({ "i", "c", "t" }, "<Leader>sk", "<Plug>(skkeleton-toggle)", { noremap = true })
end

return M
