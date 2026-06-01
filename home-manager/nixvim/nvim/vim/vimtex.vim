" vimtex completion integration
augroup user_vimtex_completefunc
  autocmd!
  autocmd FileType tex,bib setlocal completefunc=vimtex#complete#omnifunc
augroup END
