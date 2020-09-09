set number "行番号を表示する
set title "編集中のファイル名を表示
set showmatch "括弧入力時の対応する括弧を表示
set tabstop=4 "インデントをスペース4つ分に設定
set cursorline
set hlsearch
set wildmenu
set wildmode=full
set laststatus=2
set ignorecase "大文字/小文字の区別なく検索する
set smartcase "検索文字列に大文字が含まれている場合は区別して検索する
set wrapscan "検索時に最後まで行ったら最初に戻る
set ambiwidth=double " □や○文字が崩れる問題を解決
set splitbelow "新しいウィンドウを下に開く
set splitright "新しいウィンドウを右に開く
set noswapfile
set nobackup
set noundofile
set clipboard=unnamed

set fileencodings=utf-8
set laststatus=2
set fileformats=unix,dos,mac

set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2
set autoindent
set smartindent
set incsearch
set termguicolors

highlight Pmenu ctermfg=15 ctermbg=darkcyan guifg=black guibg=darkcyan
highlight PmenuSel ctermfg=white ctermbg=darkgray guibg=darkgray

nnoremap k gk
nnoremap gk k
nnoremap j gj
nnoremap gj j
nnoremap : ;
nnoremap ; :

" changed leader key to space
let mapleader = " "
nnoremap <C-c> :set hlsearch!<CR>
" スペース + . でvimrcを開く
nnoremap <Leader>. :new ~/.vimrc<CR>

set list
set listchars=tab:>-

if has('persistent_undo')
  let undo_path = expand('~/.vim/undo')
    exe 'set undodir=' .. undo_path
    set undofile
endif

if has('vim_starting')
  set rtp+=~/.vim/plugged/vim-plug
  if !isdirectory(expand('~/.vim/plugged/vim-plug'))
    echo 'install vim-plug...'
    call system('mkdir -p ~/.vim/plugged/vim-plug')
    call system('git clone https://github.com/junegunn/vim-plug.git ~/.vim/plugged/vim-plug/autoload')
  endif
endif


call plug#begin('~/.vim/plugged')
" インストールしたいプラグインを列挙 (以下は一例)
Plug 'twitvim/twitvim'
Plug 'jeetsukumaran/vim-nefertiti'
Plug 'Shougo/vimproc.vim'
Plug 'Shougo/unite.vim'
Plug 'Shougo/neomru.vim'
Plug 'Shougo/neocomplete.vim'
Plug 'Yggdroot/indentLine'
Plug 'itchyny/lightline.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'vim-jp/vimdoc-ja'
Plug 'bronson/vim-trailing-whitespace'
Plug 'NLKNguyen/papercolor-theme'
Plug 'mattn/vim-sonictemplate'
Plug 'airblade/vim-gitgutter'
Plug 'thinca/vim-quickrun'
Plug 'tpope/vim-markdown',{'for':'markdown'}
Plug 'kannokanno/previm',{'for':'markdown'}
Plug 'tyru/open-browser.vim',{'for':'markdown'}
Plug 'lambdalisue/battery.vim'
Plug 'lambdalisue/wifi.vim'
Plug 'itchyny/vim-gitbranch'
if has('nvim')
  Plug 'Shougo/defx.nvim',{ 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/defx.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
Plug 'ryanoasis/vim-devicons'
Plug 'kristijanhusak/defx-git'
Plug 'kristijanhusak/defx-icons'
Plug 't9md/vim-quickhl'
Plug 'mattn/vim-lexiv'
" ...

call plug#end()

" Required:
filetype plugin indent on
syntax enable

""" sonictemplate{{{
  let g:sonictemplate_vim_template_dir = [
    \ '$HOME/dotfiles/template'
  \]
"""}}}

 " settings of defx{{{
autocmd BufWritePost * call defx#redraw()
autocmd BufEnter * call defx#redraw()
"autocmd VimEnter * execute 'Defx'
nnoremap <silent> <C-n> :Defx<CR>
call defx#custom#option('_', {
      \ 'winwidth': 30,
      \ 'split': 'vertical',
      \ 'direction': 'topleft',
      \ 'show_ignored_files': 1,
      \ 'buffer_name': 'exlorer',
      \ 'toggle': 1,
      \ 'resume': 1,
      \ 'columns': 'indent:git:icons:filename:mark',
      \ })
autocmd FileType defx call s:defx_my_settings()

function! s:defx_my_settings() abort
  nnoremap <silent><buffer><expr> <CR>
   \ defx#do_action('drop')
  nnoremap <silent><buffer><expr> c
  \ defx#do_action('copy')
  nnoremap <silent><buffer><expr> m
  \ defx#do_action('move')
  nnoremap <silent><buffer><expr> p
  \ defx#do_action('paste')
  nnoremap <silent><buffer><expr> l
  \ defx#do_action('drop')
  nnoremap <silent><buffer><expr> t
  \ defx#do_action('open','tabnew')
  nnoremap <silent><buffer><expr> E
  \ defx#do_action('drop', 'vsplit')
  nnoremap <silent><buffer><expr> P
  \ defx#do_action('drop', 'pedit')
  nnoremap <silent><buffer><expr> o
  \ defx#do_action('open_or_close_tree')
  nnoremap <silent><buffer><expr> K
  \ defx#do_action('new_directory')
  nnoremap <silent><buffer><expr> N
  \ defx#do_action('new_file')
  nnoremap <silent><buffer><expr> M
  \ defx#do_action('new_multiple_files')
  nnoremap <silent><buffer><expr> C
  \ defx#do_action('toggle_columns',
  \                'mark:indent:icon:filename:type:size:time')
  nnoremap <silent><buffer><expr> S
  \ defx#do_action('toggle_sort', 'time')
  nnoremap <silent><buffer><expr> d
  \ defx#do_action('remove')
  nnoremap <silent><buffer><expr> r
  \ defx#do_action('rename')
  nnoremap <silent><buffer><expr> !
  \ defx#do_action('execute_command')
  nnoremap <silent><buffer><expr> x
  \ defx#do_action('execute_system')
  nnoremap <silent><buffer><expr> yy
  \ defx#do_action('yank_path')
  nnoremap <silent><buffer><expr> .
  \ defx#do_action('toggle_ignored_files')
  nnoremap <silent><buffer><expr> ;
  \ defx#do_action('repeat')
  nnoremap <silent><buffer><expr> h
  \ defx#do_action('cd', ['..'])
  nnoremap <silent><buffer><expr> ~
  \ defx#do_action('cd')
  nnoremap <silent><buffer><expr> q
  \ defx#do_action('quit')
  nnoremap <silent><buffer><expr> <Space>
  \ defx#do_action('toggle_select') . 'j'
  nnoremap <silent><buffer><expr> *
  \ defx#do_action('toggle_select_all')
  nnoremap <silent><buffer><expr> j
  \ line('.') == line('$') ? 'gg' : 'j'
  nnoremap <silent><buffer><expr> k
  \ line('.') == 1 ? 'G' : 'k'
  nnoremap <silent><buffer><expr> <C-l>
  \ defx#do_action('redraw')
  nnoremap <silent><buffer><expr> <C-g>
  \ defx#do_action('print')
  nnoremap <silent><buffer><expr> cd
  \ defx#do_action('change_vim_cwd')
endfunction

call defx#custom#column('git', 'indicators', {
  \ 'Modified'  : '✹',
  \ 'Staged'    : '✚',
  \ 'Untracked' : '✭',
  \ 'Renamed'   : '➜',
  \ 'Unmerged'  : '═',
  \ 'Ignored'   : '☒',
  \ 'Deleted'   : '✖',
  \ 'Unknown'   : '?'
  \ })
 "}}}

" color scheme
colorscheme PaperColor
set background=dark


" setting of lightline{{{
set laststatus=2
if !has('gui_running')
    set t_Co=256
endif

let g:lightline = {
      \ 'colorscheme': 'PaperColor',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'modified', 'cwd' ] ],
      \   'right':[['gitbranch', 'wifi', 'battery']]
      \ },
      \ 'component_function': {
      \    'cwd': 'getcwd',
      \    'gitbranch': 'gitbranch#name',
      \    'wifi': 'wifi#component',
      \    'battery': 'battery#component',
      \ },
      \}
" }}}


"setting about lsp{{{
if empty(globpath(&rtp, 'autoload/lsp.vim'))
  finish
endif

" setting of buffer
nnoremap <silent> <C-j> :bprev<CR>
nnoremap <silent> <C-k> :bnext<CR>

" setting for help
set helplang=ja

""" markdown {{{
  autocmd BufRead,BufNewFile *.mkd  set filetype=markdown
  autocmd BufRead,BufNewFile *.md  set filetype=markdown
  nnoremap <silent> <C-p> :PrevimOpen<CR>
  " 自動で折りたたまないようにする
  let g:vim_markdown_folding_disabled=1
  let g:previm_enable_realtime = 1
" }}}


"""setting of indenline{{{
  let g:indentLine_char_list = ['|', '¦', '┆', '┊']
"}}}

"setting of gitgutter{{{
"" 目印行を常に表示する
if exists('&signcolumn')  " Vim 7.4.2201
  set signcolumn=yes
else
  let g:gitgutter_sign_column_always = 1
endif
"}}}

" setting of quickrun{{{

let g:quickrun_config = get(g:, 'quickrun_config', {})
let g:quickrun_config._ = {
      \ 'runner'    : 'vimproc',
      \ 'runner/vimproc/updatetime' : 60,
      \ 'outputter' : 'error',
      \ 'outputter/error/success' : 'buffer',
      \ 'outputter/error/error'   : 'quickfix',
      \ 'outputter/buffer/split'  : ':rightbelow 8sp',
      \ 'outputter/buffer/close_on_empty' : 1,
      \ }
" }}}
