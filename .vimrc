" Breaks compatibility with vi
set nocompatible

" Prevent security exploits
set modelines=0
set title titlestring=

" Clipboard
set clipboard=unnamed
set clipboard+=unnamedplus

" Tab settings
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" Set proper tab / whitespace handling for a given programming language
if has("autocmd")
    " Enable file type search
    filetype plugin on
    " Use filetype indent
    filetype indent on
    " Consistent with the Linux Kernel Coding Style Guidelines
    autocmd FileType c,cpp,opencl set expandtab tabstop=4 shiftwidth=4 textwidth=80
    autocmd FileType python,html,css set expandtab tabstop=4 shiftwidth=4 softtabstop=4 textwidth=79
    autocmd FileType jade,sass,yaml set expandtab tabstop=2 shiftwidth=2 softtabstop=2 textwidth=79
endif

" General settings
syntax on
set nu
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

" Searching and moving settings
set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
set wrapscan

set wrap
set textwidth=79
set formatoptions=qrn1

" Make j & k behave rationally
nnoremap j gj
nnoremap k gk

au FocusLost * :wa
inoremap jk <ESC>

set autoindent
set smartindent
set cindent

" Window navigation
map <C-H> <C-W><Left>
map <C-L> <C-W><Right>
map <C-J> <C-W><Down>
map <C-K> <C-W><Up>

" Spacebar in insert mode inserts a single character
:nmap <Space> i_<Esc>r

set listchars=eol:↓,tab:→\ ,trail:↤,extends:>,precedes:<
set list

execute pathogen#infect()

" Disable folding which is default with plasticboy/vim-markdown
let g:vim_markdown_folding_disabled=1

" Ctags-related
set tags=./.tags,.tags,./tags,tags

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" Python
let g:syntastic_python_checkers=['flake8']

" Haskell
let g:syntastic_haskell_checkers = ['hlint']
let g:syntastic_hs_checkers=['ghc-mod', 'hlint']

" Tagbar
nmap <F8> :TagbarToggle<CR>
let g:tagbar_left=1
let g:tagbar_sort=0
let g:tagbar_width=25
set updatetime=500

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

" Solarized
syntax on
set background=dark
colorscheme solarized

" Nerd
map <C-n> :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.pyc$']
let NERDTreeShowHidden = 1

" FZF
set rtp+=~/.fzf

map <C-h> :History<CR>
map <C-b> :Buffers<CR>
map <C-g> :GFiles<CR>
map <C-f> :Files<CR>
map ag :Ag<CR>

let g:fzf_history = '/home/brandon/.fzf-history'
let g:fzf_colors = {
    \ 'fg':      ['fg', 'Normal'],
    \ 'bg':      ['bg', 'Normal'],
    \ 'hl':      ['fg', 'Comment'],
    \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
    \ 'bg+':     ['bg', 'Normal'],
    \ 'hl+':     ['fg', 'Statement'],
    \ 'info':    ['fg', 'PreProc'],
    \ 'border':  ['fg', 'Ignore'],
    \ 'prompt':  ['fg', 'Conditional'],
    \ 'pointer': ['fg', 'Exception'],
    \ 'marker':  ['fg', 'Keyword'],
    \ 'spinner': ['fg', 'Label'],
    \ 'header':  ['fg', 'Normal']
    \ }
