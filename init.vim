" vim: set ft=vim foldmarker={,} foldmethod=marker spell:

" System {
let g:python_host_prog = '/usr/local/bin/python3'

let mapleader          = "\<Space>"

set nocompatible

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
nmap <silent><leader>e :Errors<cr>                           " Quickfix
nmap <silent><leader>sc :so ~/.config/nvim/init.vim<cr>      " Source configuration
nmap <silent><leader>ec :edit ~/.config/nvim/init.vim<cr>    " Edit configuration
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

" HardMode {
nnoremap <leader>h<esc>:call ToggleHardMode()<cr>
" }

" Plugins {
call plug#begin('~/.nvim/plugged')

" Unite
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'Shougo/unite.vim'
Plug 'Shougo/neomru.vim'
Plug 'Shougo/deoplete.nvim'
Plug 'Shougo/vimshell.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sleuth'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'vim-airline/vim-airline'
Plug 'vimwiki/vimwiki'
Plug 'Chiel92/vim-autoformat'
Plug 'airblade/vim-gitgutter'
Plug 'gioele/vim-autoswap'
Plug 'majutsushi/tagbar'
Plug 'xolox/vim-misc'
Plug 'ludovicchabant/vim-gutentags'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-fugitive'
Plug 'ntpeters/vim-better-whitespace'
Plug 'jiangmiao/auto-pairs'
Plug 'godlygeek/tabular'
Plug 'easymotion/vim-easymotion'
Plug 'wikitopian/hardmode'
Plug 'junegunn/vim-peekaboo'

" Syntax
Plug 'scrooloose/syntastic'
Plug 'daveyarwood/vim-alda'
Plug 'fatih/vim-nginx'
Plug 'tmhedberg/SimpylFold'

" Color
Plug 'frankier/neovim-colors-solarized-truecolor-only'
Plug 'vim-airline/vim-airline-themes'

" Haskell
Plug 'itchyny/vim-haskell-indent'
Plug 'Twinside/vim-hoogle'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'eagletmt/ghcmod-vim'
Plug 'raichoo/haskell-vim'
Plug 'eagletmt/neco-ghc'

call plug#end()
" }

" Haskell {
map <silent> tw :GhcModTypeInsert<cr>
map <silent> ts :GhcModSplitFunCase<cr>
map <silent> tq :GhcModType<cr>
map <silent> te :GhcModTypeClear<cr>

let g:haskell_conceal_wide = 1
let g:haskell_conceal_bad  = 1
" }

" Tabularize {
nmap <leader>a# :Tabularize /"<cr>
vmap <leader>a# :Tabularize /"<cr>
nmap <leader>a" :Tabularize /"<cr>
vmap <leader>a" :Tabularize /"<cr>
nmap <leader>a& :Tabularize /&<cr>
vmap <leader>a& :Tabularize /&<cr>
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
nmap <leader>a,, :Tabularize /,\zs<cr>
vmap <leader>a,, :Tabularize /,\zNext s<cr>
nmap <leader>a< :Tabularize /<<cr>
nmap <leader>a<bar> :Tabularize /<bar><cr>
vmap <leader>a<bar> :Tabularize /<bar><cr>
" }

" Tagbar {
nmap <C-t> :TagbarToggle<cr>
" }

" Numbers {
nnoremap <leader>n :set nonumber!<CR>
let g:numbers_exclude = ['tagbar', 'gundo', 'minibufexpl', 'nerdtree', 'unite',
    \ 'startify', 'vimshell', 'w3m']
" }

" Autoformat {
noremap <leader>= :Autoformat<cr> :GitGutter<cr> :SyntasticCheck<cr>
let g:autoformat_autoindent = 0
" }

" Syntastic {
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list            = 0
let g:syntastic_check_on_open            = 0
let g:syntastic_check_on_wq              = 0
let g:pymode_trim_whitespaces            = 0
let g:pymode_lint_checkers               = ['flake8']
let g:syntastic_html_tidy_ignore_errors  = ['proprietary attribute ',
    \ 'trimming empty ']

au BufEnter *.yml set ft=yaml
au BufEnter *.yml,*.py,*js,*.hs,*.vim setlocal colorcolumn=81
au BufEnter *.js syntax region foldBraces start=/{/ end=/}/ transparent fold keepend extend | setlocal foldmethod=syntax | setlocal foldlevel=99

map <leader>s :SyntasticToggleMode<cr>
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
            \ '--ignore ''.virtualenv'' --ignore ''.vagrant'' '.
            \ '--ignore ''.hg'' --ignore ''.svn'' --ignore ''.git'''
let g:unite_source_grep_recursive_opt  = ''

nnoremap <leader>f :<C-u>Unite -no-split -buffer-name=files -start-insert file_rec/async:!<cr>
nnoremap <leader>r :<C-u>Unite -no-split -buffer-name=mru -start-insert file_mru<cr>
nnoremap <leader>b :<C-u>Unite -no-split -buffer-name=buffer buffer<cr>
nnoremap <leader>/ :<C-u>Unite -no-split -silent -buffer-name=ag grep<cr>
" }

" NERDTree {
map <C-n> :NERDTreeToggle<cr>

let NERDTreeIgnore     = ['\.pyc$']
let NERDTreeShowHidden = 1
" }

" Airline {
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

let g:airline_left_sep      = ''
let g:airline_left_alt_sep  = ''
let g:airline_right_sep     = ''
let g:airline_right_alt_sep = ''
let g:airline_theme         = 'solarized'
" }

" Gutentag {
let g:gutentags_ctags_executable = '/usr/local/bin/ctags'
let g:gutentags_exclude          = []
" }

" Vimwiki {
let g:vimwiki_list = [{"syntax": "markdown",
     \ "ext": ".md",
     \ "template_default": "default",
     \ "template_path": "~/Dropbox (Personal)/vimwiki/templates/",
     \ "custom_wiki2html": "vimwiki_markdown",
     \ "path_html": "~/Dropbox (Personal)/vimwiki/html/",
     \ "path": "~/Dropbox (Personal)/vimwiki/"}]

nmap <leader>wn :VimwikiDiaryNextDay<cr>
nmap <leader>wp :VimwikiDiaryPrevDay<cr>
" }

" Color {
syntax on
colorscheme solarized

let $NVIM_TUI_ENABLE_TRUE_COLOR = 1

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
" }

" Cursor {
augroup CursorLine
    au!
    au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    au WinLeave * setlocal nocursorline
augroup END
" }
