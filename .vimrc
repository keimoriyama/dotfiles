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
set clipboard=unnamed,autoselect "クリップボードの有効化
set ambiwidth=double " □や○文字が崩れる問題を解決
set splitbelow "新しいウィンドウを下に開く
set splitright "新しいウィンドウを右に開く
"余計なファイルを作成しない
set noswapfile
set nobackup
set noundofile

set fileencodings=utf-8
set laststatus=2
set fileformats=unix,dos,mac

set expandtab "タブ入力を複数の空白入力に置き換える
set tabstop=2 "画面上でタブ文字が占める幅
set shiftwidth=2 "自動インデントでずれる幅
set softtabstop=2 "連続した空白に対してタブキーやバックスペースキーでカーソルが動く幅
set autoindent "改行時に前の行のインデントを継続する
set smartindent "改行時に入力された行の末尾に合わせて次の行のインデントを増減する
set incsearch

nnoremap k gk
nnoremap gk k
nnoremap j gj
nnoremap gj j

" changed leader key to space
let mapleader = " "
" F3でハイライトの設定
nnoremap <C-c> :set hlsearch!<CR>

" スペース + wでファイル保存
nnoremap <Leader>w :w<CR>

nnoremap <Leader>q :q<CR>
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
Plug 'wakatime/vim-wakatime'
Plug 'jeetsukumaran/vim-nefertiti'
Plug 'Shougo/vimproc.vim'
Plug 'Shougo/unite.vim'
Plug 'Shougo/neomru.vim'
Plug 'Shougo/neocomplete.vim'
Plug 'Yggdroot/indentLine'
Plug 'itchyny/lightline.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'vim-jp/vimdoc-ja'
Plug 'bronson/vim-trailing-whitespace'
Plug 'joshdick/onedark.vim'
Plug 'mattn/vim-sonictemplate'
Plug 'airblade/vim-gitgutter'
Plug 'thinca/vim-quickrun'
Plug 'tpope/vim-markdown',{'for':'markdown'}
Plug 'kannokanno/previm',{'for':'markdown'}
Plug 'tyru/open-browser.vim',{'for':'markdown'}
Plug 'lambdalisue/battery.vim'
Plug 'lambdalisue/wifi.vim'
Plug 'itchyny/vim-gitbranch'
" ...

call plug#end()

" Required:
filetype plugin indent on
syntax enable

" setting of lightline{{{
set laststatus=2
if !has('gui_running')
    set t_Co=256
endif

let g:lightline = {
      \ 'colorscheme': 'onedark',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'modified', 'cwd' ] ],
      \   'right':[['gitbranch', 'wifi', 'battery']]
      \ },
      \ 'component_function': {
      \   'helloworld': 'Hello, world!',
      \    'cwd': 'getcwd',
      \    'gitbranch': 'gitbranch#name',
      \    'wifi': 'wifi#component',
      \    'battery': 'battery#component',
      \ },
      \}
" }}}


let g:wakatime_PythonBinary = '/usr/bin/python'  " (Default: 'python')

"setting about lsp{{{
if empty(globpath(&rtp, 'autoload/lsp.vim'))
  finish
endif

" color scheme
colorscheme onedark
let g:onedark_termcolors=256


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

""" sonictemplate{{{
  let g:sonictemplate_vim_template_dir = [
    \ '~/dotfiles/template'
  \]
"""}}}

"""setting of indenline{{{
  let g:indentLine_char_list = ['|', '¦', '┆', '┊']
"}}}

"setting of ale{{{
"" エラー行に表示するマーク
let g:ale_sign_error = '⨉'
let g:ale_sign_warning = '⚠'
" エラー行にカーソルをあわせた際に表示されるメッセージフォーマット
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
" " エラー表示の列を常時表示
let g:ale_sign_column_always = 1
"
" " ファイルを開いたときにlint実行
let g:ale_lint_on_enter = 1
" " ファイルを保存したときにlint実行
let g:ale_lint_on_save = 1
" " 編集中のlintはしない
let g:ale_lint_on_text_changed = 'never'
"
" " lint結果をロケーションリストとQuickFixには表示しない
" " 出てると結構うざいしQuickFixを書き換えられるのは困る
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 0
let g:ale_open_list = 0
let g:ale_keep_list_window_open = 0
"
" " 有効にするlinter
let g:ale_linters = {
\   'python': ['flake8'],
\}
"}}}
"
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
" 開いた箇所を自動的に作業ディレクトリにする
 augroup auto_lcd
     au!
       "au BufEnter * if &buftype !=# 'terminal' | lcd %:p:h | endif
     augroup End
""""""""""""""""""""""""""""""
" Unit.vimの設定
""""""""""""""""""""""""""""""
" 入力モードで開始する
let g:unite_enable_start_insert=1
" バッファ一覧
noremap <C-P> :Unite buffer<CR>
" ファイル一覧
noremap <C-N> :Unite -buffer-name=file file<CR>
" 最近使ったファイルの一覧
noremap <C-Z> :Unite file_mru<CR>
" sourcesを「今開いているファイルのディレクトリ」とする
noremap :uff :<C-u>UniteWithBufferDir file -buffer-name=file<CR>
" ウィンドウを分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-J> unite#do_action('split')
au FileType unite inoremap <silent> <buffer> <expr> <C-J> unite#do_action('split')
" ウィンドウを縦に分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-K> unite#do_action('vsplit')
au FileType unite inoremap <silent> <buffer> <expr> <C-K> unite#do_action('vsplit')
" ESCキーを2回押すと終了する
au FileType unite nnoremap <silent> <buffer> <ESC><ESC> :q<CR>
au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>:q<CR>
