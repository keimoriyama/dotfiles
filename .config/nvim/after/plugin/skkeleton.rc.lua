vim.fn["skkeleton#config"]({ globalJisyo = "~/.local/skkeleton/SKK-JISYO.L", eggLikeNewline = true })
vim.keymap.set({ "i", "c" }, "<C-j>", "<Plug>(skkeleton-toggle)", { noremap = true })
