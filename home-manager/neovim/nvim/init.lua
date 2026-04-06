vim.loader.enable()
require("options")
require("keymap")
require("utils")
require("dpp_config")
vim.cmd([[
let s:save_rtp = &runtimepath
set rtp-=$VIMRUNTIME
autocmd SourcePre */plugin/* ++once let &runtimepath = s:save_rtp
]])
