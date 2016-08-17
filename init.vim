" vim: set ft=vim foldmarker={,} foldmethod=marker spell:

" System {
let g:python_host_prog='/usr/local/bin/python3'

let mapleader = ","

set nocompatible

set clipboard=unnamed
set clipboard+=unnamedplus
set linebreak

" Formatting
set autoindent      " Indent at the same level of the previous line
set shiftwidth=4    " Use indents of 4 spaces
set expandtab       " Tabs are spaces, not tabs
set tabstop=4       " An indentation every four columns
set softtabstop=4   " Let backspace delete indent
set nojoinspaces    " Prevents inserting two spaces after punctuation on a join
set splitright      " Puts new vsplit windows to the right of the current
set splitbelow
set hidden
set number

" Normalize tabs
filetype plugin indent on

" SpellCheck
set spell! spelllang=en_us

" Infinite undo
if has('persistent_undo')
    set undofile
    set undodir=$HOME/.nvim/undo
endif

" Convenience helper functions
nmap <silent><leader>q :Errors<cr>                           " Quickfix
nmap <silent><leader>sc :so ~/.config/nvim/init.vim<cr>      " Source configuration
nmap <silent><leader>ec :edit ~/.config/nvim/init.vim<cr>    " Edit configuration
nmap <silent><leader>sp :setlocal spell! spelllang=en_us<cr> " Spellcheck
nmap <silent><leader>c :noh<cr>                              " Remove highlights
nmap <silent><leader>sw :StripWhitespace<cr>                 " Remove highlights
nmap <silent><leader>w :w<cr><esc>                           " Write

" Delete helper functions
nnoremap <leader>d "_d      " Delete line but do not save to register
vnoremap <leader>d "_d      " Delete line but do not save to register
vnoremap <leader>p "_dP     " Paste line but do not save to register
" }

" HardMode {
nnoremap <leader>h<esc>:call ToggleHardMode()<cr>      " Hardmode
" }

" Plugins {
call plug#begin('~/.nvim/plugged')

" Color schemes
Plug 'chriskempson/base16-vim'
Plug 'frankier/neovim-colors-solarized-truecolor-only'
Plug 'vim-airline/vim-airline-themes'

" Unite
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'Shougo/unite.vim'
Plug 'Shougo/neoyank.vim'
Plug 'Shougo/neomru.vim'
Plug 'Shougo/deoplete.nvim'

" Tools
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sensible'
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
Plug 'myusuf3/numbers.vim'
Plug 'easymotion/vim-easymotion'

" Writing
Plug 'dbmrq/vim-ditto'

" Python folding
Plug 'tmhedberg/SimpylFold'

" Syntax
Plug 'scrooloose/syntastic'
Plug 'daveyarwood/vim-alda'
Plug 'fatih/vim-nginx'

" Haskell
Plug 'itchyny/vim-haskell-indent'
Plug 'Twinside/vim-hoogle'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'eagletmt/ghcmod-vim'
Plug 'raichoo/haskell-vim'
Plug 'eagletmt/neco-ghc'

" Etc
Plug 'wikitopian/hardmode'

call plug#end()
" }

" Ditto {
au FileType markdown,text,tex DittoOn  " Turn on Ditto's autocmds
map <leader>di :ToggleDitto<cr>
" }

" Haskell {
map <silent> tw :GhcModTypeInsert<cr>
map <silent> ts :GhcModSplitFunCase<cr>
map <silent> tq :GhcModType<cr>
map <silent> te :GhcModTypeClear<cr>
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
nnoremap <leader>nt :NumbersToggle<cr>
nnoremap <leader>no :NumbersOnOff<cr>

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
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:pymode_lint_checkers = ['flake8']
let g:pymode_trim_whitespaces = 0
let g:syntastic_html_tidy_ignore_errors=['proprietary attribute ', 'trimming empty ']

au BufEnter *.yml set ft=yaml
au BufEnter *.yml,*.py,*js,*.hs,*.vim setlocal colorcolumn=81
au BufEnter *.js syntax region foldBraces start=/{/ end=/}/ transparent fold keepend extend | setlocal foldmethod=syntax | setlocal foldlevel=99

map <Leader>s :SyntasticToggleMode<cr>
" }

" Deoplete {
let g:deoplete#enable_at_startup = 1
" }

" Unite {
let g:unite_source_history_yank_enable = 1
let g:unite_source_grep_command = 'ag'
let g:unite_source_grep_default_opts = '-i --vimgrep --hidden --nocolor --nogroup ' .
            \ '--ignore ''.virtualenv'' --ignore ''.vagrant'' '.
            \ '--ignore ''.hg'' --ignore ''.svn'' --ignore ''.git'''

call unite#custom#source('file_rec,file_rec/async', 'ignore_pattern', '\(\.a$\|\.so$\|\.dyn_o$\|\.dyn_hi$\|\.dump-hi$\|\.hi$\)')
call unite#filters#matcher_default#use(['matcher_fuzzy'])

nnoremap <leader>f :<C-u>Unite -no-split -buffer-name=files -start-insert file_rec/async:!<cr>
nnoremap <leader>r :<C-u>Unite -no-split -buffer-name=mru -start-insert file_mru<cr>
nnoremap <leader>y :<C-u>Unite -no-split -buffer-name=yank history/yank<cr>
nnoremap <leader>b :<C-u>Unite -no-split -buffer-name=buffer buffer<cr>
nnoremap <leader>/ :<C-u>Unite -no-split -silent -buffer-name=ag grep<cr>
" }

" NERDTree {
map <C-n> :NERDTreeToggle<cr>

let NERDTreeIgnore = ['\.pyc$']
let NERDTreeShowHidden = 1
" }

" Airline {
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''

let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

let g:airline_theme = 'solarized'
" }

" Gutentag {
let g:gutentags_ctags_executable = '/usr/local/bin/ctags'
let g:gutentags_exclude = []
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

let hour = strftime("%H")
if 6 <= hour && hour < 18
    set background=light
else
    set background=dark
endif

colorscheme solarized

let $NVIM_TUI_ENABLE_TRUE_COLOR=1
let g:solarized_termtrans = 1
let g:solarized_termcolors = 256
let g:solarized_degrade = 0
let g:solarized_bold = 0
" }

" Cursor {
augroup CursorLine
    au!
    au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    au WinLeave * setlocal nocursorline
augroup END
" }
