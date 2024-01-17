-- lua_source {{{
local snip_dir = vim.fn.expand("$HOME/.config/nvim/snippets")
for _, file in ipairs(vim.fn.glob(snip_dir .. "/*.toml", false, true)) do
	vim.fn["denippet#load"](file)
end
vim.cmd([[
inoremap <C-l> <Plug>(denippet-expand)
inoremap <expr> <Tab> denippet#jumpable() ? '<Plug>(denippet-jump-next)' : '<Tab>'
snoremap <expr> <Tab> denippet#jumpable() ? '<Plug>(denippet-jump-next)' : '<Tab>'
inoremap <expr> <S-Tab> denippet#jumpable(-1) ? '<Plug>(denippet-jump-prev)' : '<S-Tab>'
snoremap <expr> <S-Tab> denippet#jumpable(-1) ? '<Plug>(denippet-jump-prev)' : '<S-Tab>'
]])
-- }}}
