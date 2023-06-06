local status, neo_tree = pcall(require, "neo-tree")
if not status then
	return
end

neo_tree.setup({
	close_if_last_window = false,
	popup_border_style = "rounded",
	enable_git_status = true,
	document_symbols = {
		kinds = {
			File = { icon = "󰈙", hl = "Tag" },
			Namespace = { icon = "󰌗", hl = "Include" },
			Package = { icon = "󰏖", hl = "Label" },
			Class = { icon = "󰌗", hl = "Include" },
			Property = { icon = "󰆧", hl = "@property" },
			Enum = { icon = "󰒻", hl = "@number" },
			Function = { icon = "󰊕", hl = "Function" },
			String = { icon = "󰀬", hl = "String" },
			Number = { icon = "󰎠", hl = "Number" },
			Array = { icon = "󰅪", hl = "Type" },
			Object = { icon = "󰅩", hl = "Type" },
			Key = { icon = "󰌋", hl = "" },
			Struct = { icon = "󰌗", hl = "Type" },
			Operator = { icon = "󰆕", hl = "Operator" },
			TypeParameter = { icon = "󰊄", hl = "Type" },
			StaticMethod = { icon = "󰠄 ", hl = "Function" },
		},
	},
})

local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<C-n>", "<cmd>NeoTreeShowToggle<cr>", opts)
vim.keymap.set("n", "<Leader>gs", "<cmd>Neotree float git_status<cr>", opts)
-- vim.keymap.set("n", "<C-o>", "<cmd>Neotree document_symbols<cr>", opts)
