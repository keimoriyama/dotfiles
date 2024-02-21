-- lua_add {{{
local opt = { noremap = true, silent = true }
-- ff
vim.keymap.set("n", "<Leader>ff", "<cmd>Ddu -name=ff file_rec -ui-param-ff-startFilter=v:true<cr>", opt)
-- helper
vim.keymap.set("n", "<leader>h", "<cmd>Ddu help -ui-param-ff-startFilter=v:true<cr>", opt)
-- rgの設定
local path = vim.fn.getcwd()

vim.keymap.set("n", "<leader>fr", "<cmd>Ddu rg -source-option-rg-volatile<cr>", opt)
vim.keymap.set("n", "<leader>sr", "<cmd>Ddu lsp_references -sync=true<cr>", opt)
vim.keymap.set("n", "<leader>dd", "<cmd>Ddu lsp_diagnostic -sync=true<cr>", opt)
vim.keymap.set("n", "<leader>ds", "<cmd>Ddu lsp_documentSymbol -name=lsp:hierarchy<cr>", opt)
vim.keymap.set("n", "<leader>ic", "<cmd>Ddu lsp_callHierarchy -sync=true -source-params=inComingCalls<cr>", opt)
vim.keymap.set("n", "<leader>oc", "<cmd>Ddu lsp_callHierarchy -sync=true -source-params=outgoingCalls<cr>", opt)

vim.keymap.set("n", "<Leader>sb", "<cmd>Ddu buffer<cr>", opt)
vim.keymap.set("n", "/", "<cmd>Ddu line -resume=v:false -ui-param-ff-startFilter=v:true<cr>", opt)
vim.keymap.set("n", "*", "<cmd>Ddu line -input=`expand('<cword>')` -ui-param-ff-startFilter=v:false<cr>", opt)
vim.keymap.set("n", "<leader>k", "<cmd>Ddu keymaps<cr>", opt)
vim.keymap.set("n", "<leader>dp", "<cmd>Ddu dpp<cr>", opt)
vim.keymap.set("n", "n", "<cmd>Ddu -resume=v:true<cr>", opt)
-- }}}

-- lua_source {{{
vim.fn["ddu#custom#load_config"](vim.fn.expand("~/.config/nvim/rc/ddu.ts"))
-- }}}

-- lua_post_upadte{{{
vim.fn["ddu#set_static_import_path"]()
-- }}}
