" vim: set ft=vim foldmarker={,} foldmethod=marker spell:

" System {
let g:python_host_prog = '/usr/local/bin/python3'

let mapleader = "\<Space>"

set nocompatible

set hidden
set expandtab

set tabstop=4
set shiftwidth=4
set clipboard=unnamed
set clipboard+=unnamedplus

syntax on
filetype plugin indent on

" Infinite undo
if has('persistent_undo')
    set undofile
    set undodir=$HOME/.nvim/undo
endif
" }

" Convenience mappings {
nmap <silent><leader>sc :so ~/.config/nvim/init.vim<cr>      " Source conf
nmap <silent><leader>ec :edit ~/.config/nvim/init.vim<cr>    " Edit conf
nmap <silent><leader>sp :setlocal spell! spelllang=en_us<cr> " Toggle spellcheck
nmap <silent><leader>c :noh \| lclose<cr>                    " Remove highlights and closes QuickFix window
nmap <silent><leader>w :w<cr><esc>                           " Write file
nmap <silent><leader>n :set nonumber!<cr>                    " Toggle line numbers
nmap <silent><leader>m :messages<cr>                         " Show messages
nmap <silent><leader>pc :PlugClean<cr>                       " Plugin clean
nmap <silent><leader>pi :PlugInstall<cr>                     " Plugin install
nmap <silent><leader>t :terminal<cr>                         " Terminal

nnoremap <silent><leader>sw :let _s=@/ <bar> :%s/\s\+$//e <bar> :let @/=_s <bar> :nohl <bar> :unlet _s <cr>

nnoremap <leader>d "_d      " Normal delete line but do not save to register
vnoremap <leader>d "_d      " Visual delete line but do not save to register
nnoremap <leader>p "_dP     " Normal paste line but do not save to register
vnoremap <leader>p "_dP     " Visual paste line but do not save to register
" }

" Plugins {
call plug#begin('~/.nvim/plugged')

" Tools
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'neomake/neomake'
Plug 'itchyny/lightline.vim'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'mhinz/vim-signify'
Plug 'gioele/vim-autoswap'
Plug 'jiangmiao/auto-pairs'
Plug 'xolox/vim-easytags'
Plug 'xolox/vim-misc'
Plug 'Shougo/deoplete.nvim'
Plug 'Chiel92/vim-autoformat'

" Org
Plug 'reedes/vim-pencil'
Plug 'mhinz/vim-startify'
Plug 'vimwiki/vimwiki'

" Meta
Plug 'wikitopian/hardmode'

" Haskell
Plug 'neovimhaskell/haskell-vim'
Plug 'Twinside/vim-haskellFold'

" Syntax
Plug 'daveyarwood/vim-alda'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'fatih/vim-nginx'

" Themes
Plug 'frankier/neovim-colors-solarized-truecolor-only'

call plug#end()
" }

" EasyTags {
let g:easytags_cmd = '/usr/local/bin/ctags'
" }

" Autoformat {
noremap <leader>= :Autoformat \| :SignifyRefresh<cr>

let g:autoformat_verbosemode            = 0
let g:autoformat_autoindent             = 1
let g:autoformat_retab                  = 1
let g:autoformat_remove_trailing_spaces = 1
" }

" EasyAlign {
noremap <leader>e :EasyAlign<cr>
" }

" Neomake {
autocmd! BufWritePost * Neomake
let g:neomake_open_list   = 2
let g:neomake_place_signs = 0
" }

" Deoplete {
let g:deoplete#enable_at_startup = 1
" }

" FZF {
nnoremap <leader>h :History<cr>
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>g :GFiles<cr>
nnoremap <leader>f :Files<cr>
nnoremap <leader>/ :Ag<cr>

let g:fzf_history_dir = '~/.fzf-history'
" }

" NERDTree {
map <C-n> :NERDTreeToggle<cr>

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let NERDTreeIgnore     = ['\.pyc$']
let NERDTreeShowHidden = 1
" }

" Airline {
let g:lightline = {
            \ 'colorscheme': 'solarized',
            \ 'active': {
            \     'left': [['mode', 'paste'], ['fugitive'], ['filename']]
            \ },
            \ 'component_function': {
            \     'fugitive': 'LLFugitive',
            \     'readonly': 'LLReadonly',
            \     'modified': 'LLModified',
            \     'filename': 'LLFilename',
            \     'mode':     'LLMode'
            \     }
            \ }

function! LLMode()
    let fname = expand('%:t')
    return fname == '__Tagbar__' ? 'Tagbar' :
                \ fname == 'ControlP' ? 'CtrlP' :
                \ lightline#mode() == 'NORMAL' ? 'N' :
                \ lightline#mode() == 'INSERT' ? 'I' :
                \ lightline#mode() == 'VISUAL' ? 'V' :
                \ lightline#mode() == 'V-LINE' ? 'V' :
                \ lightline#mode() == 'V-BLOCK' ? 'V' :
                \ lightline#mode() == 'REPLACE' ? 'R' : lightline#mode()
endfunction

function! LLModified()
    if &filetype == "help"
        return ""
    elseif &modified
        return "+"
    elseif &modifiable
        return ""
    else
        return ""
    endif
endfunction

function! LLReadonly()
    if &filetype == "help"
        return ""
    elseif &readonly
        return "!"
    else
        return ""
    endif
endfunction

function! LLFugitive()
    return exists('*fugitive#head') ? fugitive#head() : ''
endfunction

function! LLFilename()
    return ('' != LLReadonly() ? LLReadonly() . ' ' : '') .
                \ ('' != expand('%:t') ? expand('%:t') : '[No Name]') .
                \ ('' != LLModified() ? ' ' . LLModified() : '')
endfunction
" }

" Vimwiki {
let g:vimwiki_list = [{"syntax": "markdown",
            \ "ext"              : ".md",
            \ "template_default" : "default",
            \ "template_path"    : "/Users/brandon/Dropbox (Personal)/vimwiki/templates/",
            \ "custom_wiki2html" : "vimwiki_markdown",
            \ "path_html"        : "/Users/brandon/Dropbox (Personal)/vimwiki/html/",
            \ "path"             : "/Users/brandon/Dropbox (Personal)/vimwiki/"}]

nmap <leader>wn :VimwikiDiaryNextDay<cr>
nmap <leader>wp :VimwikiDiaryPrevDay<cr>
" }

" Color {
let $NVIM_TUI_ENABLE_TRUE_COLOR = 1

colorscheme solarized

let g:solarized_termtrans  = 1
let g:solarized_termcolors = 256
let g:solarized_degrade    = 0
let g:solarized_bold       = 0

set background=dark
" }

" Syntax overrides {
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufReadPost *.yml set filetype=yaml
" }

" Pencil {
let g:pencil#wrapModeDefault = 'soft'

augroup pencil
    autocmd!
    autocmd FileType markdown,mkd call pencil#init()
    autocmd FileType text         call pencil#init()
augroup END
" }

" QF {
au FileType qf call AdjustWindowHeight(3, 5)
function! AdjustWindowHeight(minheight, maxheight)
    exe max([min([line("$"), a:maxheight]), a:minheight]) . "wincmd _"
endfunction
" }

" Signify {
let g:signify_sign_show_count = 1
let g:signify_sign_show_text  = 1
" }

" Solarized {
let g:solarized_termcolors = 256
let g:solarized_termtrans  = 1
let g:solarized_degrade    = 0
let g:solarized_bold       = 0
let g:solarized_underline  = 0
let g:solarized_italic     = 1
let g:solarized_contrast   = "normal"
let g:solarized_visibility = "normal"
let g:solarized_hitrail    = 1
let g:solarized_menu       = 1
" }
