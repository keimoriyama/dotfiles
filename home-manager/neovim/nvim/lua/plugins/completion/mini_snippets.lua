local snippets = require("mini.snippets")
local gen_loader = snippets.gen_loader

snippets.setup({
	snippets = {
		gen_loader.from_lang(),
	},
	mappings = {
		expand = "<C-e>",
		jump_next = "<Tab>",
		jump_prev = "<S-Tab>",
		stop = "<C-c>",
	},
})

snippets.start_lsp_server({ match = false })
