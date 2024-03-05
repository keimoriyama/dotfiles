-- lua_source {{{
local dictdir = vim.fn.getenv("DPP_BASE") .. "/repos/github.com/skk-dev/dict"
vim.keymap.set({ "i", "c" }, "<C-j>", "<Plug>(skkeleton-enable)", { noremap = true })
vim.keymap.set({ "i", "c" }, "<C-l>", "<Plug>(skkeleton-disable)", { noremap = true })

local userDict = vim.fn.expand("$HOME") .. "/.local/skkeleton/SKK-JISYO.L"

vim.fn["skkeleton#config"]({
	globalDictionaries = {
		vim.fs.joinpath(dictdir, "SKK-JISYO.L"),
		vim.fs.joinpath(dictdir, "SKK-JISYO.edict"),
		vim.fs.joinpath(dictdir, "SKK-JISYO.edict2"),
		vim.fs.joinpath(dictdir, "SKK-JISYO.fullname"),
		vim.fs.joinpath(dictdir, "SKK-JISYO.propernoun"),
	},
	userDictionary = userDict,
	debug = false,
})
--}}}
