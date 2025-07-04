" hook_source{{{
call ddc#custom#patch_filetype(['tex'], 'sourceOptions', {
      \ 'omni': {'mark': 'O', 'forceCompletionPattern': g:vimtex#re#deoplete},
      \ })
call ddc#custom#patch_filetype(['tex'], 'sourceParams', {
      \ 'omni': {'omnifunc': 'vimtex#complete#omnifunc'},
      \ })
let g:vimtex_view_method = "skim"
let g:vimtex_view_general_viewer = "skim"
let g:vimtex_view_skim_activate = 1
let g:vimtex_view_skim_synmc = 1
"let g:vimtex_compiler_method = "latexmk"
let g:vimtex_view_compiler_latexmk_engines = [ "-", "-pdf" ]
let g:latex_latexmk_options = "-pdf"
let g:vimtex_syntax_enabled = 0
"let g:vimtex_compiler_latexmk = {
"      \ 'background': 1,
"      \ 'build_dir': '',
"      \ 'continuous': 1,
"      \ 'options': [
"      \    '-pdfdvi', 
"      \    '-verbose',
"      \    '-file-line-error',
"      \    '-synctex=1',
"      \    '-interaction=nonstopmode',
"      \],
"      \}
" }}}
