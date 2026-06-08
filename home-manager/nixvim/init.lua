local ok_skel = pcall(require, "skkeleton")
if ok_skel then
	local userDict = "~/Library/Containers/net.mtgto.inputmethod.macSKK/Data/Documents/Dictionaries/skk-jisyo.utf8"
	vim.fn["skkeleton#azik#add_table"]("us")
	vim.fn["skkeleton#register_keymap"]("henkan", "X", "")
	vim.fn["skkeleton#register_keymap"]("input", "[", "katakana")
	vim.cmd([[
    call skkeleton#register_kanatable('rom', {
            \   ',': ['，', ''],
            \   '.': ['．', ''],
            \ })
  ]])
	vim.fn["skkeleton#config"]({
		kanaTable = "azik",
		sources = { "skk_dictionary", "skk_server" },
		userDictionary = userDict,
		globalDictionaries = {
			vim.fs.joinpath(vim.fn.expand("~/.skk-dict"), "SKK-JISYO.L"),
		},
		debug = false,
	})
end
