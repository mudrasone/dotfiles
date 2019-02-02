" vim: set foldmarker={,} foldlevel=0 foldmethod=marker:

" Modeline and Notes {
"
"        ___                       ___           ___           ___
"       /\__\          ___        /\__\         /\  \         /\  \
"      /:/  /         /\  \      /::|  |       /::\  \       /::\  \
"     /:/  /          \:\  \    /:|:|  |      /:/\:\  \     /:/\:\  \
"    /:/__/  ___      /::\__\  /:/|:|__|__   /::\~\:\  \   /:/  \:\  \
"    |:|  | /\__\  __/:/\/__/ /:/ |::::\__\ /:/\:\ \:\__\ /:/__/ \:\__\
"    |:|  |/:/  / /\/:/  /    \/__/~~/:/  / \/_|::\/:/  / \:\  \  \/__/
"    |:|__/:/  /  \::/__/           /:/  /     |:|::/  /   \:\  \
"     \::::/__/    \:\__\          /:/  /      |:|\/__/     \:\  \
"      ~~~~         \/__/         /:/  /       |:|  |        \:\__\
"                                 \/__/         \|__|         \/__/
"
"
"   This is the personal .vimrc file of Pindaroso.
"   While much of it is beneficial for general use, I would
"   recommend picking out the parts you want and understand.
"
"   You can find me at https://www.github.com/pindaroso
"
"   Copyright 2018 Brandon Stiles
"
"   Licensed under the Apache License, Version 2.0 (the "License");
"   you may not use this file except in compliance with the License.
"   You may obtain a copy of the License at
"
"       http://www.apache.org/licenses/LICENSE-2.0
"
"   Unless required by applicable law or agreed to in writing, software
"   distributed under the License is distributed on an "AS IS" BASIS,
"   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
"   See the License for the specific language governing permissions and
"   limitations under the License.
" }

" Breaks compatibility with vi {
set nocompatible
" }

" Prevent security exploits {
set modelines=0
set title titlestring=
" }

" Folds {
augroup AutoSaveFolds
  autocmd!
  autocmd BufWinLeave,BufLeave,BufWritePost ?* nested silent! mkview!
  autocmd BufWinEnter ?* silent! loadview
augroup END

set viewoptions=folds,cursor
set sessionoptions=folds
" }

" Clipboard {
set clipboard=unnamed
set clipboard+=unnamedplus

nnoremap d "_d
xnoremap d "_d
xnoremap p "_dP
" }

" Session persistence {
set backupdir=$HOME/.vim/.backup//
set directory=$HOME/.vim/.swap//
set undodir=$HOME/.vim/.undo//
set undofile
" }

" Set proper tab / whitespace handling for a given programming language {
if has("autocmd")
    " Enable file type search
    filetype plugin indent on
    " Consistent with the Linux Kernel Coding Style Guidelines
    autocmd FileType c,cpp,opencl set expandtab tabstop=4 shiftwidth=4 textwidth=80
    autocmd FileType python,html,css set expandtab tabstop=4 shiftwidth=4 softtabstop=4 textwidth=79
    autocmd FileType jade,sass,yaml set expandtab tabstop=2 shiftwidth=2 softtabstop=2 textwidth=79
    autocmd BufNewFile,BufRead *.html set filetype=htmldjango
endif
" }

" General settings {
syntax on

set nu
set updatetime=500

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

set encoding=utf-8
set scrolloff=3
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
set cursorline
set ttyfast
set ruler
set backspace=indent,eol,start
set laststatus=2

set list listchars=tab:∙\ ,trail:·,precedes:←,extends:→,eol:¬,nbsp:␣

set autoindent
set smartindent
set cindent

set mouse=a
" }

" Searching and moving settings {
set smartcase
set incsearch
set showmatch
set hlsearch
set wrapscan
set wrap
set formatoptions=qrn1
" }

" Make j & k behave rationally {
nnoremap j gj
nnoremap k gk
" }

" Write {
au FocusLost * :wa
inoremap jk <ESC>
" }

" Spacebar in insert mode inserts a single character {
:nmap <Space> i_<Esc>r
" }

" Window navigation {
map <C-H> <C-W><Left>
map <C-L> <C-W><Right>
map <C-J> <C-W><Down>
map <C-K> <C-W><Up>
" }

" Ctags {
set tags=./.tags,.tags,./tags,tags
" }

execute pathogen#infect()

" Syntastic {
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_python_checkers = ['flake8']
let g:syntastic_python_checker_args="--ignore=E501,W601"
let g:syntastic_python_flake8_args="--ignore=E501,W601,W504"

let g:syntastic_haskell_checkers = ['ghc-mod', 'hlint -i "Reduce duplication"']

let g:elm_syntastic_show_warnings = 1
" }

" Tagbar {
map <C-o> :TagbarToggle<CR>

let g:tagbar_left = 0
let g:tagbar_sort = 0
let g:tagbar_width = 25
let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
        \ 'module' : 'm',
        \ 'class'  : 'c',
        \ 'data'   : 'd',
        \ 'type'   : 't'
    \ }
\ }

let g:tagbar_type_go = {
    \ 'ctagstype' : 'go',
    \ 'kinds'     : [
        \ 'p:package',
        \ 'i:imports:1',
        \ 'c:constants',
        \ 'v:variables',
        \ 't:types',
        \ 'n:interfaces',
        \ 'w:fields',
        \ 'e:embedded',
        \ 'm:methods',
        \ 'r:constructor',
        \ 'f:functions'
    \ ],
    \ 'sro' : '.',
    \ 'kind2scope' : {
        \ 't' : 'ctype',
        \ 'n' : 'ntype'
    \ },
    \ 'scope2kind' : {
        \ 'ctype' : 't',
        \ 'ntype' : 'n'
    \ }
\ }
" }

" Color {
set background=dark
let g:solarized_termtrans = 1
let g:solarized_termcolors = 16
colorscheme solarized
hi Specialkey ctermbg=8
" }

" NERDTree {
map <C-n> :NERDTreeToggle<CR>

let NERDTreeIgnore = ['\.pyc$']
let NERDTreeShowHidden = 1
" }

" FZF {
set rtp+=$HOME/.fzf

map <C-h> :History<CR>
map <C-b> :Buffers<CR>
map <C-f> :GFiles<CR>
map <C-g> :Files<CR>
map <C-k> :Ag<CR>

let g:fzf_history = $HOME + '/.fzf-history'
let g:fzf_colors = {
    \ 'fg':      ['fg', 'Normal'],
    \ 'bg':      ['bg', 'Normal'],
    \ 'hl':      ['fg', 'Comment'],
    \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
    \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
    \ 'hl+':     ['fg', 'Normal'],
    \ 'info':    ['fg', 'PreProc'],
    \ 'border':  ['fg', 'Ignore'],
    \ 'prompt':  ['fg', 'Conditional'],
    \ 'pointer': ['fg', 'Exception'],
    \ 'marker':  ['fg', 'Keyword'],
    \ 'spinner': ['fg', 'Label'],
    \ 'header':  ['fg', 'Normal']
    \ }
" }

" Spelling {
set spelllang=en_us spell

hi clear SpellBad
hi SpellBad cterm=underline
" }

" YCM {
set completeopt-=preview

let g:ycm_add_preview_to_completeopt = 0
let g:ycm_autoclose_preview_window_after_completion = 1

let g:ycm_semantic_triggers = {
     \ 'elm' : ['.'],
     \ }
" }

" FAR {
let g:far#source = "ag"
" }

" Cool {
let g:CoolTotalMatches = 1
" }

" Autoformat {
noremap <C-l> :Autoformat<CR>
" }
