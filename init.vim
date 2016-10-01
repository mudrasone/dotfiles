" vim: set ft=vim foldmarker={,} foldmethod=marker spell:

" System {
let g:python_host_prog = '/usr/local/bin/python3'

let mapleader = "\<Space>"

set nocompatible

set tabstop=4
set shiftwidth=4
set expandtab
set clipboard=unnamed
set clipboard+=unnamedplus

syntax on
filetype plugin indent on

" Infinite undo
if has('persistent_undo')
    set undofile
    set undodir=$HOME/.nvim/undo
endif

" Convenience functions
nmap <silent><leader>sc :so ~/.config/nvim/init.vim<cr>      " Source conf
nmap <silent><leader>ec :edit ~/.config/nvim/init.vim<cr>    " Edit conf
nmap <silent><leader>sp :setlocal spell! spelllang=en_us<cr> " Toggle spellcheck
nmap <silent><leader>c :noh \| lclose<cr>                    " Remove highlights
nmap <silent><leader>sw :StripWhitespace<cr>                 " Strip whitespaces
nmap <silent><leader>w :w<cr><esc>                           " Write file
nmap <silent><leader>n :set nonumber!<cr>

" Clipboard convenience functions
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
Plug 'mhinz/vim-startify'
Plug 'vimwiki/vimwiki'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'neomake/neomake'
Plug 'itchyny/lightline.vim'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'airblade/vim-gitgutter'
Plug 'gioele/vim-autoswap'
Plug 'jiangmiao/auto-pairs'
Plug 'ludovicchabant/vim-gutentags'
Plug 'Shougo/deoplete.nvim'
Plug 'Chiel92/vim-autoformat'
Plug 'reedes/vim-pencil'

" Meta
Plug 'wikitopian/hardmode'

" Haskell
Plug 'raichoo/haskell-vim'
Plug 'itchyny/vim-haskell-indent'

" Syntax
Plug 'daveyarwood/vim-alda'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'fatih/vim-nginx'

" Themes
Plug 'frankier/neovim-colors-solarized-truecolor-only'

call plug#end()
" }

" Autoformat {
noremap <leader>= :Autoformat \| :redraw<cr>

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
nnoremap <leader>/ :Files<cr>
" }

" NERDTree {
map <C-n> :NERDTreeToggle<cr>

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let NERDTreeIgnore     = ['\.pyc$']
let NERDTreeShowHidden = 1
" }

" Airline {
let g:lightline = {'colorscheme': 'solarized'}
" }

" Gutentag {
let g:gutentags_ctags_executable = '/usr/local/bin/ctags'
let g:gutentags_exclude          = []
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

" Cursor {
augroup CursorLine
    au!
    au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    au WinLeave * setlocal nocursorline
augroup END
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
