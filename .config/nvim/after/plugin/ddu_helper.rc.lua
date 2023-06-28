vim.fn["ddu#custom#patch_local"]("ff", {
	ui = "ff",
	sources = {
		{ name = "helper", params = {} },
	},
	sourceOptions = {
		_ = {
			columns = { "filename" },
			sorters = { "sorter_alpha" },
		},
	},
	kindOptions = {
		file = {
			defaultAction = "open",
		},
	},
	uiParams = {
		filer = {
			split = "floating",
		},
	},
})
