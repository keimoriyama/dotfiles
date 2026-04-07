-- lua_source {{{
local snip_dir = vim.fn.expand("$HOME/.config/nvim/snippets")
for _, file in ipairs(vim.fn.glob(snip_dir .. "/*.toml", false, true)) do
	vim.fn["denippet#load"](file)
end
for _, file in ipairs(vim.fn.glob(snip_dir .. "/*.ts", false, true)) do
	vim.fn["denippet#load"](file)
end
vim.keymap.set("i", "<C-e>", "<Plug>(denippet-expand)")
vim.keymap.set({ "i", "s" }, "<Tab>", function()
	if vim.fn["denippet#jumpable"]() then
		return "<Plug>(denippet-jump-next)"
	else
		return "<Tab>"
	end
end, { expr = true })

vim.keymap.set({ "i", "s" }, "<C-p>", function()
	if vim.fn["denippet#jumpable"](-1) then
		return "<Plug>(denippet-jump-next)"
	else
		return "<C-p>"
	end
end, { expr = true })
-- }}}
