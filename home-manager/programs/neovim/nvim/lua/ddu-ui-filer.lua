-- lua_add {{{
vim.keymap.set("n", "<Leader>sf", "<cmd>Ddu file -name=filer<cr>")
--}}}

-- lua_ddu-filer{{{
local opt = { buffer = true, silent = true }
vim.keymap.set("n", "<CR>", '<cmd>call ddu#ui#do_action("itemAction", {"name":"open"})<CR>', opt)
vim.keymap.set(
	"n",
	"<C-v>",
	'<cmd>call ddu#ui#do_action("itemAction", {"name":"open", "params":{"command":"vnew"}})<CR>',
	opt
)
vim.keymap.set(
	"n",
	"<C-h>",
	'<cmd>call ddu#ui#do_action("itemAction", {"name":"open", "params":{"command":"split"}})<CR>',
	opt
)
vim.keymap.set("n", "<Space>", '<cmd>call ddu#ui#do_action("toggleSelectItem")<CR>', opt)
vim.keymap.set("n", "o", '<cmd>call ddu#ui#do_action("expandItem", {"mode": "toggle"})<CR>', opt)
vim.keymap.set("n", "q", '<cmd>call ddu#ui#do_action("quit")<CR>', opt)
vim.keymap.set("n", "N", '<cmd>call ddu#ui#do_action("itemAction", {"name": "newFile"})<cr>', opt)
vim.keymap.set("n", "d", '<cmd>call ddu#ui#do_action("itemAction", {"name": "delete"})<cr>', opt)
vim.keymap.set("n", "r", '<cmd>call ddu#ui#do_action("itemAction", {"name": "rename"})<cr>', opt)
vim.keymap.set("n", "y", '<cmd>call ddu#ui#do_action("itemAction", {"name": "yank"})<cr>', opt)
vim.keymap.set("n", "c", '<cmd>call ddu#ui#do_action("itemAction", {"name": "copy"})<cr>', opt)
vim.keymap.set("n", "p", '<cmd>call ddu#ui#do_action("itemAction", {"name": "paste"})<cr>', opt)
-- }}}
