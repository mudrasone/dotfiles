" vim: set ft=vim foldmarker={,} foldmethod=marker spell:

" System {
set nocompatible

let g:python_host_prog = '/usr/local/bin/python3'

let mapleader = "\<Space>"

set hidden
set expandtab

set tabstop=4
set shiftwidth=4
set clipboard=unnamed
set clipboard+=unnamedplus

if has('termguicolors')
  set termguicolors
endif

syntax on
filetype plugin indent on

" Infinite undo
if has('persistent_undo')
    set undofile
    set undodir=$HOME/.nvim/undo
endif
" }

" Convenience mappings {
nmap <silent><Leader>sc :so ~/.config/nvim/init.vim<CR>      " Source conf
nmap <silent><Leader>ec :edit ~/.config/nvim/init.vim<CR>    " Edit conf
nmap <silent><Leader>sp :setlocal spell! spelllang=en_us<CR> " Toggle spellcheck
nmap <silent><Leader>c :noh<CR>                              " Remove highlights
nmap <silent><Leader>q :lclose<CR>                           " Close QuickFix
nmap <silent><Leader>w :w<CR><Esc>                           " Write file
nmap <silent><Leader>n :set nonumber!<CR>                    " Toggle line numbers
nmap <silent><Leader>m :messages<CR>                         " Show messages
nmap <silent><Leader>pc :PlugClean<CR>                       " Plugin clean
nmap <silent><Leader>pi :PlugInstall<CR>                     " Plugin install
nmap <silent><Leader>m :call ToggleBackground()<CR>          " Toggle background
nmap <silent><Leader>t :terminal<CR>

function! ToggleBackground()
    let &background = (&background == "dark" ? "light" : "dark")
    let g:lightline.colorscheme = 'gruvbox-light'
endfunction

" Automatically change working directory to buffer file
autocmd BufEnter * silent! :lcd%:p:h

nnoremap <Silent><Leader>sw :let _s=@/ <Bar> :%s/\s\+$//e <Bar>
            \ :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>

nnoremap <Leader>d "_d      " Normal delete line but do not save to register
vnoremap <Leader>d "_d      " Visual delete line but do not save to register
nnoremap <Leader>p "_dP     " Normal paste line but do not save to register
vnoremap <Leader>p "_dP     " Visual paste line but do not save to register
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
Plug 'reedes/vim-pencil'
Plug 'mhinz/vim-startify'
Plug 'vimwiki/vimwiki'
Plug 'vim-scripts/restore_view.vim'

" Meta
Plug 'wikitopian/hardmode'

" Haskell
Plug 'parsonsmatt/intero-neovim'

" Syntax
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'daveyarwood/vim-alda'
Plug 'fatih/vim-nginx'

" Theme
Plug 'morhetz/gruvbox'

call plug#end()
" }

" EasyTags {
let g:easytags_cmd = '/usr/local/bin/ctags'
" }

" Autoformat {
noremap <Leader>= :Autoformat \| :SignifyRefresh<CR>

let g:autoformat_verbosemode            = 0
let g:autoformat_autoindent             = 1
let g:autoformat_retab                  = 1
let g:autoformat_remove_trailing_spaces = 1
" }

" Intero {

" Process management
nnoremap <Leader>hio :InteroOpen<CR>
nnoremap <Leader>hik :InteroKill<CR>
nnoremap <Leader>hic :InteroHide<CR>
nnoremap <Leader>hil :InteroLoadCurrentModule<CR>

" REPL commands
nnoremap <Leader>hie :InteroEval<CR>
nnoremap <Leader>hit :InteroGenericType<CR>
nnoremap <Leader>hiT :InteroType<CR>
nnoremap <Leader>hii :InteroInfo<CR>
nnoremap <Leader>hiI :InteroTypeInsert<CR>

" Go to definition
nnoremap <Leader>hid :InteroGoToDef<CR>

" Highlight uses of identifier:
nnoremap <Leader>hiu :InteroUses<CR>

" Reload the file in Intero after saving
autocmd! BufWritePost *.hs InteroReload
" }

" EasyAlign {
noremap <Leader>e :EasyAlign<CR>
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
nnoremap <Leader>h :History<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>g :GFiles<CR>
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>/ :Ag<CR>

let g:fzf_history_dir = '~/.fzf-history'
" }

" NERDTree {
map <C-n> :NERDTreeToggle<CR>

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") &&
            \ b:NERDTree.isTabTree()) | q | endif

let NERDTreeIgnore     = ['\.pyc$']
let NERDTreeShowHidden = 1
" }

" Lightline {
let g:lightline = {
            \ 'colorscheme': 'gruvbox',
            \ 'active': {
            \     'left': [['mode', 'paste'], ['fugitive'], ['filename']]
            \ },
            \ 'subseparator': { 'left': "•", 'right': "•" },
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
    elseif &filetype == "nerdtree"
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
    elseif &filetype == "nerdtree"
        return ""
    elseif &readonly
        return "!"
    else
        return ""
    endif
endfunction

function! LLFugitive()
    try
        if expand('%:t') !~? 'Tagbar\|Gundo\|NERD' && &ft !~? 'vimfiler' && 
                    \ exists('*fugitive#head')
            let mark = ''  " edit here for cool mark
            let branch = fugitive#head()
            return branch !=# '' ? mark.branch : ''
        endif
    catch
    endtry
    return ''
endfunction

function! LLFilename()
    if &filetype == "nerdtree"
        return "NER"
    elseif &filetype == "fzf"
        return ""
    endif
    let fname = expand('%:t')
    return fname == 'ControlP' && has_key(g:lightline, 'ctrlp_item') ?
                \ g:lightline.ctrlp_item :
                \ fname == '__Tagbar__' ? g:lightline.fname :
                \ fname =~ '__Gundo\|NERD_tree\|nerdtree' ? '' :
                \ &ft == 'vimfiler' ? vimfiler#get_status_string() :
                \ &ft == 'unite' ? unite#get_status_string() :
                \ &ft == 'vimshell' ? vimshell#get_status_string() :
                \ ('' != LLReadonly() ? LLReadonly() . ' ' : '') .
                \ ('' != fname ? fname : '[No Name]') .
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

nmap <Leader>wn :VimwikiDiaryNextDay<CR>
nmap <Leader>wp :VimwikiDiaryPrevDay<CR>
" }

" Color {
let NVIM_TUI_ENABLE_TRUE_COLOR=1

colorscheme gruvbox
set background=dark
" }

" Pencil {
let g:pencil#wrapModeDefault = 'soft'

augroup pencil
    autocmd!
    autocmd FileType markdown,mkd call pencil#init()
    autocmd FileType text         call pencil#init()
augroup END
" }

" Signify {
let g:signify_sign_show_count = 1
let g:signify_sign_show_text  = 1
" }

" Restore View {
set viewoptions=cursor,folds,slash,unix
let g:skipview_files = ['Startify', 'nerdtree']
" }
