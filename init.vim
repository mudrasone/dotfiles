" vim: set ft=vim foldmarker={,} foldmethod=marker spell:

" System {
set nocompatible

let g:python3_host_prog = '/usr/local/bin/python3'
let g:python_host_prog  = '/usr/bin/python'

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

set spelllang=en_us
set spellfile=$HOME/.nvim/spell/en.utf-8.add
set complete+=kspell
autocmd BufRead,BufNewFile *.md setlocal spell

" Infinite undo
if has('persistent_undo')
    set undofile
    set undodir=$HOME/.nvim/undo
endif
" }

" Convenience mappings {
nmap <silent><Leader>pc :PlugClean<CR>
nmap <silent><Leader>pi :PlugInstall<CR>
nmap <silent><Leader>t :terminal<CR>
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
call plug#begin('~/.nvim/plugged')

" Tools
Plug 'neomake/neomake'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify'
Plug 'gioele/vim-autoswap'
Plug 'Raimondi/delimitMate'
Plug 'xolox/vim-easytags'
Plug 'xolox/vim-misc'
Plug 'Chiel92/vim-autoformat'
Plug 'airblade/vim-rooter'
Plug 'reedes/vim-pencil'

" UI
Plug 'vim-airline/vim-airline'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'Shougo/deoplete.nvim'
Plug 'Junegunn/goyo.vim'

" FP
Plug 'eagletmt/neco-ghc'
Plug 'neovimhaskell/haskell-vim'
Plug 'let-def/vimbufsync', { 'on': 'CoqLaunch' }
Plug 'the-lambda-church/coquille', { 'branch': 'pathogen-bundle', 'on': 'CoqLaunch' }

" Meta
Plug 'vimwiki/vimwiki'
Plug 'mhinz/vim-startify'

" Syntax
Plug 'LnL7/vim-nix'
Plug 'tpope/vim-markdown'
Plug 'jvoorhis/coq.vim'
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

" Haskell {
let g:haskellmode_completion_ghc = 0
let g:necoghc_enable_detailed_browse = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
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
map <Leader>cn :CoqNext<CR>
nmap <Leader>cp :CoqPrevious<CR>
" }

" Obsession {
nmap <Leader>s :call ToggleObsess()<CR>

function! ToggleObsess()
    let d=FindRootDirectory()."/.session.vim"
    if !empty(glob(d))
        echo "Loading previous session..."
        source d
    else
        echo "Creating new session..."
        Obsess d
    endif
endfunction
" }

" Vim-rooter {
let g:rooter_change_directory_for_non_project_files = 'current'
let g:rooter_patterns = ['stack.yaml', '*.cabal', '.git', '.git/']
" }

" Goyu {
nmap <Leader>yo :Goyo<CR>
" }

" Markdown {
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'coq']
" }
