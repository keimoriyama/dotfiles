-- lua_source {{{
vim.g.memolist_path = "$HOME/Documents/memo"
vim.g.memolist_memo_suffix = "md"
local opt = { noremap = true, silent = true }
vim.keymap.set("n", "<leader>mn", "<cmd>MemoNew<CR>", opt)
-- vim.keymap.set("n", "<leader>ml", "<cmd>MemoList<CR>", opt)
vim.keymap.set("n", "<leader>mg", "<cmd>MemoGrep<CR>", opt)
--}}}
