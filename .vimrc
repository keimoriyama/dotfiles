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
	Plug 'Shougo/ddc.vim'
	Plug 'shun/ddc-vim-lsp'
	" filer plugins
	Plug 'lambdalisue/fern.vim'
	Plug 'lambdalisue/fern-renderer-nerdfont.vim'
	Plug 'lambdalisue/fern-git-status.vim'
	Plug 'lambdalisue/glyph-palette.vim'
	Plug 'lambdalisue/nerdfont.vim'
	" git plugins
call plug#end()

if has('persistent_undo')
	let undo_path = expand('~/.vim/undo')
	exe 'set undodir=' .. undo_path
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

" オペレーター待機モードのマッピング
" 一例：c8 で ci( の動きになる
onoremap 8 i(
onoremap 2 i"
onoremap 7 i'
onoremap @ i`
onoremap [ i[
onoremap { i{

" Ctrl + j と Ctrl + k で 段落の前後に移動
nnoremap <C-j> }
nnoremap <C-k> {

" TODO: add keymap by plugins
