local spec = {
	{
		"vim-skk/skkeleton",
		event = { "InsertEnter" },
		dependencies = { "skkeleton_indicator", "skkeleton-azik-kanatable", "vim-denops/denops.vim" },
		config = function()
			local dictdir = vim.fn.expand("$HOME") .. "/.cache/lazy/dict"
			vim.keymap.set({ "i", "c" }, "<C-j>", "<Plug>(skkeleton-enable)", { noremap = true })

			vim.keymap.set({ "i", "c" }, "<C-l>", "<Plug>(skkeleton-disable)", { noremap = true })

			local userDict = vim.fn.expand("$HOME") .. "/.local/skkeleton/SKK-JISYO.L"

			vim.fn["skkeleton#azik#add_table"]("us")

			vim.fn["skkeleton#config"]({
				kanaTable = "azik",
			})

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
		end,
	},
	{ "vim-denops/denops.vim" },
	{
		"skk-dev/dict",
	},
	{
		"delphinus/skkeleton_indicator.nvim",
		config = function()
			require("skkeleton_indicator").setup()
		end,
	},
	{ "NI57721/skkeleton-azik-kanatable", branch = "feature/delete-some-mappings" },
}

return spec
