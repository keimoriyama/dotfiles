vim.g.vimtex_view_method = "skim"
vim.g.vimtex_view_general_viewer = "skim"
vim.g.vimtex_compiler_method = "latexmk"
vim.g.vimtex_view_compiler_latexmk_engines = { "-", "-pdf" }
vim.g.latex_latexmk_options = "-pdf"
vim.g.vimtex_syntax_enabled = 0

vim.cmd([[
let g:vimtex_compiler_latexmk = {
            \ 'background' : 0,
            \ 'build_dir' : '',
            \ 'continuous' : 1,
            \ 'options' : [
            \   '-pdf',
            \   '-verbose',
            \   '-file-line-error',
            \   '-synctex=1',
            \   '-interaction=nonstopmode',
            \ ],
            \}
]])

vim.cmd([[
let g:vimtex_quickfix_ignore_filters = [
      \ 'Underfull',
      \ 'Overfull',
	  \ 'Package transparent'
      \]
]])

vim.cmd('let maplocalleader = " "')
