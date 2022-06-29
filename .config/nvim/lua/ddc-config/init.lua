vim.cmd[[
  call ddc#custom#patch_global('sourceOptions', {
      \ '_': {
      \   'matchers': ['matcher_fuzzy','matcher_head','matcher_length'],
      \   'sorters': ['sorter_fuzzy','sorter_rank'],
      \   'converters': ['converter_fuzzy']
      \ },
      \ 'around': {'mark': 'A'},
      \ 'nvim-lsp': {'mark': 'lsp','forceCompletionPattern': '\.\w*|:\w*|->\w*' },
      \ 'file': {'mark': 'F', 'isVolatile': v:true,'forceCompletionPattern': '\S/\S*'},
      \ 'buffer': {'mark': 'B'},
      \ 'path': {'mark': 'P'},
      \ })
  call ddc#custom#patch_global('sourceParams', {
      \ 'around': {'maxSize': 500},
      \ 'nvim-lsp': { 'kindLabels': { 'Class': 'c' } },
      \ 'buffer': {'requireSameFiletype': v:false},
      \ 'path': {'cmd': ['fd','--max-depth','5']},
      \ })
]]
-- -- Use ddc.
vim.fn['ddc#enable']() 

-- Use signature help
vim.fn['signature_help#enable']()

-- use pop up preview
vim.fn['popup_preview#enable']()
