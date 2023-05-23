local neogit = require("neogit")

neogit.setup({})
local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<C-g>", "<cmd>Neogit<CR>", opts)
