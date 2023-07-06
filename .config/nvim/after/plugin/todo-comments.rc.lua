local status, todocomments = pcall(require, "todo-comments")
if not status then
	return
end

todocomments.setup({})
local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<Leader>tq", "<cmd>TodoQuickFix<cr>", opts)
