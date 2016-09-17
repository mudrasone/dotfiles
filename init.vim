" vim: set ft=vim foldmarker={,} foldmethod=marker spell:

" System {
let g:python_host_prog = '/usr/local/bin/python3'

let mapleader = "\<Space>"

set nocompatible
syntax on

set clipboard=unnamed
set clipboard+=unnamedplus

set spell! spelllang=en_us

filetype plugin indent on

" Infinite undo
if has('persistent_undo')
    set undofile
    set undodir=$HOME/.nvim/undo
endif

" Convenience helper functions
nmap <silent><leader>sc :so ~/.config/nvim/init.vim<cr>      " Source conf
nmap <silent><leader>ec :edit ~/.config/nvim/init.vim<cr>    " Edit conf
nmap <silent><leader>sp :setlocal spell! spelllang=en_us<cr> " Toggle spellcheck
nmap <silent><leader>c :noh<cr>                              " Remove highlights
nmap <silent><leader>sw :StripWhitespace<cr>                 " Strip whitespaces
nmap <silent><leader>w :w<cr><esc>                           " Write file

" Clipboard helper functions
nnoremap <leader>d "_d      " Normal delete line but do not save to register
vnoremap <leader>d "_d      " Visual delete line but do not save to register
nnoremap <leader>p "_dP     " Normal paste line but do not save to register
vnoremap <leader>p "_dP     " Visual paste line but do not save to register
" }

" Plugins {
call plug#begin('~/.nvim/plugged')

" Utils
Plug 'Shougo/vimproc.vim', { 'do': 'make' }

" Tools
Plug 'Shougo/unite.vim'
Plug 'Shougo/neomru.vim'
Plug 'Shougo/deoplete.nvim'
Plug 'neomake/neomake'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sleuth'
Plug 'Chiel92/vim-autoformat'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'itchyny/lightline.vim'
Plug 'vimwiki/vimwiki'
Plug 'airblade/vim-gitgutter'
Plug 'gioele/vim-autoswap'
Plug 'ludovicchabant/vim-gutentags'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-fugitive'
Plug 'ntpeters/vim-better-whitespace'
Plug 'jiangmiao/auto-pairs'
Plug 'godlygeek/tabular'
Plug 'mhinz/vim-startify'
Plug 'tmhedberg/SimpylFold'
Plug 'Chiel92/vim-autoformat'

" Meta
Plug 'wikitopian/hardmode'

" Syntax
Plug 'daveyarwood/vim-alda'
Plug 'fatih/vim-nginx'

" Themes
Plug 'frankier/neovim-colors-solarized-truecolor-only'
Plug 'jacoborus/tender.vim'

" Haskell
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'raichoo/haskell-vim'
Plug 'itchyny/vim-haskell-indent'
Plug 'Twinside/vim-hoogle'
Plug 'eagletmt/ghcmod-vim'
Plug 'eagletmt/neco-ghc'

call plug#end()
" }

" Haskell {
map <silent> tw :GhcModTypeInsert<cr>
map <silent> ts :GhcModSplitFunCase<cr>
map <silent> tq :GhcModType<cr>
map <silent> te :GhcModTypeClear<cr>
" }

" Tabularize {
nmap <leader>a= :Tabularize /^[^=]*\zs=<cr>
vmap <leader>a= :Tabularize /^[^=]*\zs=<cr>
nmap <leader>a=> :Tabularize /=><cr>
vmap <leader>a=> :Tabularize /=><cr>
nmap <leader>a: :Tabularize /:<cr>
vmap <leader>a: :Tabularize /:<cr>
nmap <leader>a:: :Tabularize /:\zs<cr>
vmap <leader>a:: :Tabularize /:\zs<cr>
nmap <leader>a, :Tabularize /,<cr>
vmap <leader>a, :Tabularize /,<cr>
" }

" Numbers {
nnoremap <leader>n :set nonumber!<CR>
" }

" Autoformat {
noremap <leader>= :Autoformat redraw<cr>
let g:formatters_python = ["/usr/local/bin/autopep8"]
" }

" Neomake {
autocmd! BufWritePost * Neomake
let g:neomake_open_list   = 2
let g:neomake_place_signs = 0
" }

" Deoplete {
let g:deoplete#enable_at_startup = 1
" }

" Unite {
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
call unite#custom#source('file_rec,file_rec/async', 'ignore_pattern',
            \ '\(\.a$\|\.so$\|\.dyn_o$\|\.dyn_hi$\|\.dump-hi$\|\.hi$\)')

let g:unite_source_grep_command        = 'ag'
let g:unite_source_grep_default_opts   = '-i --vimgrep --hidden --nocolor --nogroup ' .
            \ '--ignore ''.virtualenv'' --ignore ''.vagrant'' ' .
            \ '--ignore ''.hg'' --ignore ''.svn'' --ignore ''.git'''
let g:unite_source_grep_recursive_opt  = ''

nnoremap <leader>f :<C-u>Unite -no-split -buffer-name=files -start-insert file_rec/async:!<cr>
nnoremap <leader>r :<C-u>Unite -no-split -buffer-name=mru -start-insert file_mru<cr>
nnoremap <leader>b :<C-u>Unite -no-split -buffer-name=buffer buffer<cr>
nnoremap <leader>/ :<C-u>Unite -no-split -silent -buffer-name=ag grep<cr>
" }

" NERDTree {
map <C-n> :NERDTreeToggle<cr>

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let NERDTreeIgnore     = ['\.pyc$']
let NERDTreeShowHidden = 1
" }

" Airline {
let g:lightline        = {'colorscheme': 'tender'}
let g:tender_lightline = 1
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

if 0
    colorscheme solarized

    let g:solarized_termtrans  = 1
    let g:solarized_termcolors = 256
    let g:solarized_degrade    = 0
    let g:solarized_bold       = 0
    let hour                   = strftime("%H")

    if 6 <= hour && hour < 18
        set background=light
    else
        set background=dark
    endif
else
    colorscheme tender
endif
" }

" Cursor {
augroup CursorLine
    au!
    au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    au WinLeave * setlocal nocursorline
augroup END
" }

" Markdown {
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
" }

" Column {
au BufEnter *.py setlocal colorcolumn=81
" }
