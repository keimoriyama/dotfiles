set fileencodings=utf-8
set relativenumber
set hlsearch
set incsearch
set smartindent
set clipboard+=unnamed
set laststatus=2
set wildmenu
set autoindent
set autoread
set noswapfile
syntax enable
set tabstop=4
set shiftwidth=4
set modifiable

let data_dir = has("nvim") ? stdpath("data") . "/site" : "~/.vim"

if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute "!curl -fLo ".data_dir."/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
	" lsp settings
	Plug 'prabirshrestha/vim-lsp'
	Plug 'mattn/vim-lsp-settings'
	" auto complete plugins
	Plug 'prabirshrestha/asyncomplete.vim'
	Plug 'prabirshrestha/asyncomplete-lsp.vim'
	" filer plugins
	Plug 'lambdalisue/fern.vim'
	Plug 'lambdalisue/fern-renderer-nerdfont.vim'
	Plug 'lambdalisue/fern-git-status.vim'
	Plug 'yuki-yano/fern-preview.vim'
	Plug 'lambdalisue/glyph-palette.vim'
	Plug 'lambdalisue/nerdfont.vim'
	" git plugins
	Plug 'tpope/vim-fugitive'
	" color scheme
	Plug 'lifepillar/vim-solarized8'
	" statusline
	Plug 'itchyny/lightline.vim'
	" plugin for tex
	Plug 'lervag/vimtex'
call plug#end()


if has('persistent_undo')
	let undo_dir = has("nvim") ? stdpath("data") . "~/.nvim" : "~/.vim"
	exe 'set undodir=' .. undo_dir
	set undofile
endif

autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif


" <Leader>というプレフィックスキーにスペースを使用する
let g:mapleader = "\<Space>"

" スペース + wでファイル保存
nnoremap <Leader>w :w<CR>

" カーソル下の単語を、置換後の文字列の入力を待つ状態にする
nnoremap <Leader>re :%s;\<<C-R><C-W>\>;g<Left><Left>;

" Escを2回押すとハイライトを消す
nnoremap <Esc><Esc> :nohlsearch<CR>

" スペース + . でvimrcを開く
nnoremap <Leader>. :new ~/.vimrc<CR>

" Ctrl + j と Ctrl + k で 段落の前後に移動
nnoremap <C-j> }
nnoremap <C-k> {

" TODO: add keymap by plugins
" setting of fern
let g:fern#default_hidden=1
let g:fern#renderer = 'nerdfont'
nnoremap <C-n> :Fern . -reveal=% -drawer -toggle -width=30<CR>
function! s:fern_settings() abort
  nmap <silent> <buffer> p     <Plug>(fern-action-preview:toggle)
  nmap <silent> <buffer> <C-p> <Plug>(fern-action-preview:auto:toggle)
  nmap <silent> <buffer> <C-d> <Plug>(fern-action-preview:scroll:down:half)
  nmap <silent> <buffer> <C-u> <Plug>(fern-action-preview:scroll:up:half)
endfunction
augroup fern-settings
	autocmd!
	autocmd FileType fern call s:fern_settings()
augroup END

" setting color scheme
colorscheme solarized8

" setting of status line
if !has('gui_running')
	set t_Co=256
endif

let g:lightline = {
	\ 'colorscheme': 'solarized',
    \ 'component': {
    \   'filename': '%n:%t'
    \ }
\ }

" setting of fugitive.vim
nmap <silent> <Leader>ga :Gwrite<CR>
nmap <silent> <Leader>gc :Git commit<CR>
nmap <silent> <Leader>gl :Git log<CR>
nmap <silent> <Leader>gd :Git diff<CR>
nmap <silent> <Leader>gs :Git<CR>
nmap <silent> <Leader>gb :Git blame<CR>

" setting of vimtex
let g:latex_latexmk_options = '-pdf'
