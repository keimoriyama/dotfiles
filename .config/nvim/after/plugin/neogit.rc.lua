local neogit = require("neogit")

neogit.setup({ disable_commit_confirmation = true })
local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<Leader>g", "<cmd>Neogit<CR>", opts)
