" vim: set ft=vim foldmarker={,} foldmethod=marker spell:

" System {
set nocompatible

let g:python3_host_prog = '/usr/local/bin/python3'
let g:python_host_prog = '/usr/bin/python'

let mapleader = "\<Space>"

set hidden expandtab
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
nmap <silent><Leader>sc :so ~/.config/nvim/init.vim<CR>      " Source config
nmap <silent><Leader>ec :edit ~/.config/nvim/init.vim<CR>    " Edit config
nmap <silent><Leader>sp :setlocal spell! spelllang=en_us<CR> " Toggle spellcheck
nmap <silent><Leader>c :noh<CR>                              " Remove highlights
nmap <silent><Leader>q :lclose<CR>                           " Close QuickFix
nmap <silent><Leader>w :w<CR><Esc>                           " Write file
nmap <silent><Leader>n :set nonumber!<CR>                    " Toggle line numbers
nmap <silent><Leader>pc :PlugClean<CR>                       " Plugin clean
nmap <silent><Leader>pi :PlugInstall<CR>                     " Plugin install
nmap <silent><Leader>t :terminal<CR>

" Strip whitespace
nnoremap <silent><Leader>sw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<Bar>:unlet _s<CR>

" Override copy and paste
nnoremap <Leader>d "_d      " Normal delete line but do not save to register
vnoremap <Leader>d "_d      " Visual delete line but do not save to register
nnoremap <Leader>p "_dP     " Normal paste line but do not save to register
vnoremap <Leader>p "_dP     " Visual paste line but do not save to register
" }

" Plugins {
call plug#begin('~/.nvim/plugged')

" Tools
Plug 'tpope/vim-surround'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'neomake/neomake'
Plug 'vim-airline/vim-airline'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify'
Plug 'gioele/vim-autoswap'
Plug 'Raimondi/delimitMate'
Plug 'xolox/vim-easytags'
Plug 'xolox/vim-misc'
Plug 'Shougo/deoplete.nvim'
Plug 'Chiel92/vim-autoformat'
Plug 'reedes/vim-pencil'
Plug 'eagletmt/neco-ghc'
Plug 'neovimhaskell/haskell-vim'

" Meta
Plug 'vimwiki/vimwiki'
Plug 'mhinz/vim-startify'

" Syntax
Plug 'LnL7/vim-nix'
Plug 'alx741/vim-yesod'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'daveyarwood/vim-alda'
Plug 'fatih/vim-nginx'

" Theme
Plug 'frankier/neovim-colors-solarized-truecolor-only'
Plug 'vim-airline/vim-airline-themes'

call plug#end()
" }

" Tags {
au BufWritePost *.hs silent !init-tags %
au BufWritePost *.hsc silent !init-tags %

let g:easytags_cmd = '/usr/local/bin/ctags'
" }

" AutoFormat {
noremap <Leader>= :Autoformat \| :SignifyRefresh<CR>

let g:autoformat_verbosemode            = 0
let g:autoformat_autoindent             = 1
let g:autoformat_retab                  = 1
let g:autoformat_remove_trailing_spaces = 1
" }

" EasyAlign {
noremap <Leader>e :EasyAlign<CR>
" }

" FZF {
nnoremap <Leader>h :History<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>g :GFiles<CR>
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>/ :Ag<CR>

let g:fzf_history_dir = '~/.fzf-history'
let g:fzf_colors = {
            \ 'fg':      ['fg', 'Normal'],
            \ 'hl':      ['fg', 'Normal'],
            \ 'fg+':     ['fg', 'Normal'],
            \ 'bg':      ['bg', 'Normal'],
            \ 'bg+':     ['bg', 'Normal', 'Label'],
            \ 'hl+':     ['fg', 'Normal'],
            \ 'info':    ['fg', 'PreProc'],
            \ 'prompt':  ['fg', 'Conditional'],
            \ 'pointer': ['fg', 'Normal'],
            \ 'marker':  ['fg', 'Keyword'],
            \ 'spinner': ['fg', 'Label'],
            \ 'header':  ['fg', 'Normal']
            \ }
" }

" Neomake {
autocmd! BufWritePost * Neomake

let g:neomake_open_list   = 2
let g:neomake_place_signs = 0
" }

" Deoplete {
let g:deoplete#enable_at_startup = 1
" }

" NERDTree {
noremap <C-n> :NERDTreeToggle<CR>

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") &&
            \ b:NERDTree.isTabTree()) | q | endif

let NERDTreeIgnore     = ['\.pyc$']
let NERDTreeShowHidden = 1
" }

" Vimwiki {
let g:vimwiki_list = [{
            \ "syntax":           "markdown",
            \ "ext":              ".md",
            \ "template_default": "default",
            \ "template_path":    "/Users/brandon/Dropbox (Personal)/vimwiki/templates/",
            \ "custom_wiki2html": "vimwiki_markdown",
            \ "path_html":        "/Users/brandon/Dropbox (Personal)/vimwiki/html/",
            \ "path":             "/Users/brandon/Dropbox (Personal)/vimwiki/"
            \ }]

nmap <Leader>wn :VimwikiDiaryNextDay<CR>
nmap <Leader>wp :VimwikiDiaryPrevDay<CR>
" }

" Airline {
let g:airline_theme                           = 'solarized'
let g:airline#extensions#tabline#left_sep     = ' '
let g:airline#extensions#tabline#left_alt_sep = ' '
let g:airline_left_alt_sep                    = '-'
let g:airline_right_alt_sep                   = '-'
let g:airline_left_sep                        = ''
let g:airline_right_sep                       = ''
" }

" Color {
colorscheme solarized

let g:solarized_contrast = 'medium'
set background=dark
" }

" Pencil {
let g:pencil#wrapModeDefault = 'soft'

augroup pencil
    autocmd!
    autocmd FileType md,markdown,mkd call pencil#init()
    autocmd FileType text call pencil#init()
augroup END
" }

" Signify {
let g:signify_sign_show_count = 1
let g:signify_sign_show_text  = 1
" }
