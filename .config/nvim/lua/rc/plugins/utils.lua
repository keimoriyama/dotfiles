local spec = {
{"nvim-lua/plenary.nvim"},
{
"ellisonleao/gruvbox.nvim",
lazy=false,
config = function()
vim.cmd([[
set background=dark
colorscheme gruvbox
]])
end
},
}

return spec
