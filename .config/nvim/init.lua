require 'plugins'
require 'keymap'
vim.cmd [[autocmd BufWritePost plugins.lua source <afile> | PackerCompile]]
vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.formatting_sync()]]
