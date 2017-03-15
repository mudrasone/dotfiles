" vim: set ft=vim foldmarker={,} foldmethod=marker spell:

" System {
set nocompatible

let mapleader = "\<Space>"

let g:python3_host_prog = '/usr/bin/python3'
let g:python_host_prog  = '/usr/bin/python'

set clipboard=unnamed,unnamedplus
set termguicolors

syntax on
filetype plugin indent on

" Tabs
set tabstop=4
set shiftwidth=4
set expandtab

" Spelling
set spelllang=en_us
set spellfile=$HOME/.nvim/spell/en.utf-8.add
set complete+=kspell

" Infinite undo
if has('persistent_undo')
    set undofile
    set undodir=$HOME/.nvim/undo
endif

" Plugin shortcuts
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
call plug#begin('~/.nvim/plugged')

" Tools
Plug 'neomake/neomake'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'mhinz/vim-signify'
Plug 'gioele/vim-autoswap'
Plug 'Raimondi/delimitMate'
Plug 'xolox/vim-easytags'
Plug 'xolox/vim-misc'
Plug 'Chiel92/vim-autoformat'

" UI
Plug 'vim-airline/vim-airline'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'Shougo/deoplete.nvim'

" FP
Plug 'dag/vim2hs'
Plug 'bitc/vim-hdevtools'
Plug 'neovimhaskell/haskell-vim'
Plug 'jvoorhis/coq.vim'

" Writing
Plug 'vimwiki/vimwiki'
Plug 'mhinz/vim-startify'
Plug 'reedes/vim-pencil'
Plug 'ChesleyTan/wordCount.vim'
Plug 'Junegunn/goyo.vim'
Plug 'jamessan/vim-gnupg'

" Syntax
Plug 'LnL7/vim-nix'
Plug 'alx741/vim-yesod'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'daveyarwood/vim-alda'
Plug 'fatih/vim-nginx'
Plug 'nowk/genericdc'

" Theme
Plug 'frankier/neovim-colors-solarized-truecolor-only'
Plug 'vim-airline/vim-airline-themes'

call plug#end()
" }

" Tags {
let g:easytags_cmd = '/usr/local/bin/ctags'

au BufWritePost *.hs,*.hsc,*.lhs silent !init-tags %
" }

" Haskell {
let g:hdevtools_options              = '-g-isrc -g-Wall'
let g:neomake_haskell_enabled_makers = ['hlint']

au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>
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
let g:rg_command = '
            \ rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --color "always"
            \ -g "*.{js,json,php,md,styl,jade,html,config,py,cpp,c,go,hs,rb,conf}"
            \ -g "!{.git,node_modules,vendor,docs}/*" '

command! -bang -nargs=* F call fzf#vim#grep(g:rg_command .shellescape(<q-args>), 1, <bang>0)

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
let NERDTreeShowHidden = 1

noremap <C-n> :NERDTreeToggle<CR>
" }

" Vimwiki {
let g:vimwiki_list = [{
            \ "syntax": "markdown",
            \ "ext":    ".gpg",
            \ "path":   "/Users/brandon/Dropbox (Personal)/vimwiki/"
            \ }]

nmap <Leader>wn :VimwikiDiaryNextDay<CR>
nmap <Leader>wp :VimwikiDiaryPrevDay<CR>
" }

" Airline {
let g:airline_theme         = 'solarized'
let g:airline_left_alt_sep  = '-'
let g:airline_right_alt_sep = '-'
let g:airline_left_sep      = ''
let g:airline_right_sep     = ''
" }

" Color {
set background=dark

silent colorscheme solarized
" }

" Pencil {
let g:pencil#wrapModeDefault = 'soft'

nmap <Leader>words :WordCount<CR>

autocmd BufRead,BufNewFile,BufEnter *.markdown,*.md,*.gpg setlocal spell
autocmd BufRead,BufNewFile,BufEnter *.markdown,*.md,*.gpg call pencil#init()
" }

" Signify {
let g:signify_sign_show_count = 1
let g:signify_sign_show_text  = 1
" }

" Goyu {
nmap <Leader>yo :Goyo <bar> highlight StatusLineNC ctermfg=white<CR>
" }

" GPG {
let g:GPGUseAgent        = 1
let g:GPGPreferSymmetric = 1
let g:GPGPreferArmor     = 1
let g:GPGUsePipes        = 1
" }
