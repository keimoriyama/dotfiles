local neogit = require("neogit")

neogit.setup({ disable_commit_confirmation = true })
local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<C-g>", "<cmd>Neogit<CR>", opts)
