-- lua_source {{{
require("nvim-treesitter").setup({})

pcall(vim.treesitter.start)
-- local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
-- parser_config.tsx.filetype_to_parsername = { "javascript", "typescript.tsx" }
vim.o.foldexpr = "v:lua.vim.treesitter.foldexpr()"
-- }}}
