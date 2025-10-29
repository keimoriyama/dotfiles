-- lua_source {{{
vim.keymap.set({ "i", "c" }, "<C-j>", "<Plug>(skkeleton-enable)", { noremap = true })

vim.keymap.set({ "i", "c" }, "<C-l>", "<Plug>(skkeleton-disable)", { noremap = true })

-- local userDict = vim.fn.expand("$HOME") .. "/Documents/skk-jisyo.utf-8"
local userDict = "~/Library/Containers/net.mtgto.inputmethod.macSKK/Data/Documents/Dictionaries/skk-jisyo.utf8"

vim.fn["skkeleton#azik#add_table"]("us")
vim.fn["skkeleton#register_keymap"]("henkan", "X", "")

vim.cmd([[
  call skkeleton#register_kanatable('rom', {
          \   ',': ['，', ''],
          \   '.': ['．', ''],
          \ })
]])

local dictdir = ".skk-dict/"
vim.fn["skkeleton#config"]({
	kanaTable = "azik",
	-- sources = {
	-- 	"skk_server"
	-- },
	userDictionary = userDict,
	globalDictionaries = {
		vim.fs.joinpath(dictdir, "SKK-JISYO.L"),
		vim.fs.joinpath(dictdir, "SKK-JISYO.edict"),
		vim.fs.joinpath(dictdir, "SKK-JISYO.edict2"),
		vim.fs.joinpath(dictdir, "SKK-JISYO.fullname"),
		vim.fs.joinpath(dictdir, "SKK-JISYO.propernoun"),
	},
	debug = false,
})
--}}}
