vim.loader.enable()

-- Load core configurations
require("core.options")
require("core.keymaps")
require("core.autocmds")

-- Load utilities
require("utils")

-- Load plugin manager and plugin configurations
require("plugins")

vim.cmd([[
let s:save_rtp = &runtimepath
set rtp-=$VIMRUNTIME
autocmd SourcePre */plugin/* ++once let &runtimepath = s:save_rtp
]])
