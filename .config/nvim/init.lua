require'plugins'
require'keymap'
vim.cmd[[autocmd BufWritePost plugins.lua source <afile> | PackerCompile]]
