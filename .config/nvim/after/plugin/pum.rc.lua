vim.cmd([[
inoremap <silent><expr> <TAB>
      \ pum#visible() ? '<Cmd>call pum#map#insert_relative(+1)<CR>' :
      \ (col('.') <= 1 <Bar><Bar> getline('.')[col('.') - 2] =~# '\s') ?
      \ '<TAB>' : ddc#map#manual_complete()
]])

vim.keymap.set("i", "<S-Tab>", "<cmd>call pum#map#insert_relative(-1)<cr>")
vim.keymap.set("i", "<C-n>", "<cmd>call pum#map#insert_relative(+1)<cr>")
vim.keymap.set("i", "<C-p>", "<cmd>call pum#map#insert_relative(-1)<cr>")
vim.keymap.set("i", "<C-y>", "<cmd>call pum#map#confirm()<cr>")
vim.keymap.set("i", "<C-e>", "<cmd>call pum#map#cancel()<cr>")
