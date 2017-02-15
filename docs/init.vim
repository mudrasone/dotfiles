" vim: set ft=vim foldmarker={,} foldmethod=marker spell:

" System {
let g:python3_host_prog = '/usr/bin/python3'
set clipboard=unnamedplus

let g:python_host_prog  = '/usr/bin/python'

let mapleader = "\<Space>"

set termguicolors

if has('syntax') && !exists('g:syntax_on')
  syntax enable
endif

filetype plugin indent on

" Tabs
set tabstop=4
set shiftwidth=4
set expandtab

set spelllang=en_us
set spellfile=$HOME/.nvim/spell/en.utf-8.add
set complete+=kspell

" Infinite undo
if has('persistent_undo')
    set undofile
    set undodir=$HOME/.nvim/undo
endif
" }

" Convenience mappings {
nmap <silent><Leader>pc :PlugClean<CR>
nmap <silent><Leader>pi :PlugInstall<CR>
" Source neovim config
nmap <silent><Leader>sc :so ~/.config/nvim/init.vim<CR>
" Edit neovim config
nmap <silent><Leader>ec :edit ~/.config/nvim/init.vim<CR>
" Toggle spellcheck
nmap <silent><Leader>sp :setlocal spell! spelllang=en_us<CR>
" Remove highlights
nmap <silent><Leader>c :noh<CR>
" Close QuickFix
nmap <silent><Leader>q :lclose<CR>
nmap <silent><Leader>w :w<CR><Esc>
" Toggle line numbers
nmap <silent><Leader>n :set nonumber!<CR>
" Update spellfile
nmap <silent><Leader>ms :mkspell! ~/.nvim/spell/en.utf-8.add<CR>
" Edit spellfile
nmap <silent><Leader>es :edit ~/.nvim/spell/en.utf-8.add<CR>
" Strip whitespace
nnoremap <silent><Leader>sw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<Bar>:unlet _s<CR>

" Normal delete line but do not save to register
nnoremap <Leader>d "_d
" Visual delete line but do not save to register
vnoremap <Leader>d "_d
" Normal paste line but do not save to register
nnoremap <Leader>p "_dP
" Visual paste line but do not save to register
vnoremap <Leader>p "_dP
" }

" Plugins {
call plug#begin("~/.config/nvim/plugged")

" Tools
Plug 'neomake/neomake'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify'
Plug 'gioele/vim-autoswap'
Plug 'Raimondi/delimitMate'
Plug 'xolox/vim-easytags'
Plug 'xolox/vim-misc'
Plug 'Chiel92/vim-autoformat'
Plug 'airblade/vim-rooter'
Plug 'tpope/vim-surround'

" UI
Plug 'vim-airline/vim-airline'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'Shougo/deoplete.nvim'

" FP
Plug 'dag/vim2hs'
Plug 'bitc/vim-hdevtools'
Plug 'let-def/vimbufsync'
Plug 'jvoorhis/coq.vim'
Plug 'the-lambda-church/coquille', { 'branch': 'pathogen-bundle','on': 'CoqLaunch' }
Plug 'neovimhaskell/haskell-vim'

" Writing
Plug 'vimwiki/vimwiki'
Plug 'mhinz/vim-startify'
Plug 'reedes/vim-pencil'
Plug 'Junegunn/goyo.vim'

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
let g:easytags_cmd = '/usr/local/bin/ctags'

au BufWritePost *.hs silent !init-tags %
au BufWritePost *.hsc silent !init-tags %
" }

" Haskell {
au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>

let g:hdevtools_options              = '-g-isrc -g-Wall'
let g:neomake_haskell_enabled_makers = ['hlint']
" }

" HTML {
let g:neomake_html_maker = {
    \ 'exe': 'tidy',
    \ 'args': ['--no-color', '--preset', 'airbnb', '--reporter', 'inline', '--esnext'],
    \ 'errorformat': '%f: line %l\, col %c\, %m',
    \ }
" }

" AutoFormat {
let g:autoformat_verbosemode            = 0
let g:autoformat_autoindent             = 1
let g:autoformat_retab                  = 1
let g:autoformat_remove_trailing_spaces = 1

noremap <Leader>= :Autoformat \| :SignifyRefresh<CR>
" }

" EasyAlign {
noremap <Leader>e :EasyAlign<CR>
" }

" FZF {
let g:fzf_history_dir = '~/.fzf-history'

let g:fzf_colors = { 'fg':      ['fg', 'Normal'],
            \ 'bg':      ['bg', 'Normal'],
            \ 'hl':      ['fg', 'Comment'],
            \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
            \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
            \ 'hl+':     ['fg', 'Statement'],
            \ 'info':    ['fg', 'PreProc'],
            \ 'prompt':  ['fg', 'Conditional'],
            \ 'pointer': ['fg', 'Exception'],
            \ 'marker':  ['fg', 'Keyword'],
            \ 'spinner': ['fg', 'Label'],
            \ 'header':  ['fg', 'Comment'] }

nnoremap <Leader>h :History<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>g :GFiles<CR>
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>/ :Ag<CR>
" }

" Neomake {
let g:neomake_open_list   = 2
let g:neomake_place_signs = 0

autocmd! BufWritePost * Neomake
" }

" Deoplete {
let g:deoplete#enable_at_startup = 1
" }

" NERDTree {
let NERDTreeIgnore     = ['\.pyc$']
let NERDTreeShowHidden = 1

noremap <C-n> :NERDTreeToggle<CR>

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") &&
            \ b:NERDTree.isTabTree()) | q | endif
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

" Markdown {
autocmd BufRead,BufNewFile *.md setlocal spell
" }

" Airline {
let g:airline_theme         = 'solarized'
let g:airline_left_alt_sep  = '-'
let g:airline_right_alt_sep = '-'
let g:airline_left_sep      = ''
let g:airline_right_sep     = ''
" }

" Color {
"silent! colorscheme solarized

set background=dark
" }

" Pencil {
let g:pencil#wrapModeDefault = 'soft'

augroup pencil
    setlocal spell
    autocmd!
    autocmd FileType markdown,mkd call pencil#init()
    autocmd FileType text call pencil#init()
augroup END
" }

" Signify {
let g:signify_sign_show_count = 1
let g:signify_sign_show_text  = 1
" }

" Coq {
au FileType coq call coquille#CoqideMapping()

nmap <Leader>cl :CoqLaunch<CR>
nmap <Leader>cn :CoqNext<CR>
nmap <Leader>ck :CoqKill<CR>
nmap <Leader>cu :CoqUndo<CR>
nmap <Leader>cc :CoqToCursor<CR>
" }

" Obsession {
nmap <Leader>s :Obsess<CR>
" }

" Vim-Rooter {
let g:rooter_change_directory_for_non_project_files = 'current'
let g:rooter_patterns                               = ['.git', '.git/', 'stack.yaml', '.projectfile']
let g:rooter_silent_chdir                           = 1
" }

" Goyu {
nmap <Leader>yo :Goyo <bar> highlight StatusLineNC ctermfg=white<CR>
" }
