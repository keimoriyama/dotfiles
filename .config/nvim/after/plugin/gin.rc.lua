local option = { noremap = true, silent = true }

vim.api.nvim_set_keymap("n", "<leader>gb", "<cmd>GinBranch<cr>", option)
vim.api.nvim_set_keymap("n", "<leader>gd", "<cmd>GinDiff<cr>", option)
vim.api.nvim_set_keymap("n", "<leader>gl", "<cmd>GinLog<cr>", option)
vim.api.nvim_set_keymap("n", "<leader>gs", "<cmd>GinStatus<cr>", option)
