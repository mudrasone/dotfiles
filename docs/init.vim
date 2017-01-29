" vim: set ft=vim foldmarker={,} foldmethod=marker spell:

" System {
set nocompatible

if system('uname -s') == "Darwin\n"
  " OSX
  set clipboard=unnamed
  let g:python3_host_prog = '/usr/local/bin/python3'
else
  " Linux
  let g:python3_host_prog = '/usr/bin/python3'
  set clipboard=unnamedplus
endif

let g:python_host_prog  = '/usr/bin/python'

let mapleader = "\<Space>"

set termguicolors

syntax on
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

" UI
Plug 'vim-airline/vim-airline'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'Shougo/deoplete.nvim'

" FP
Plug 'eagletmt/neco-ghc'
Plug 'neovimhaskell/haskell-vim'
Plug 'let-def/vimbufsync'
Plug 'jvoorhis/coq.vim'
Plug 'the-lambda-church/coquille', { 'branch': 'pathogen-bundle' } " , 'on': 'CoqLaunch' }

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
Plug 'kchmck/vim-coffee-script'

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
let g:haskellmode_completion_ghc     = 0
let g:necoghc_enable_detailed_browse = 1

autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
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
function! has#colorscheme(name)
    pat = 'colors/'.a:name.'.vim'
    return !empty(globpath(&rtp, pat))
endfunction

" .vimrc
if has#colorscheme('solarized')
    colorscheme solarized
    let g:solarized_contrast = 'high'
endif

nmap <Leader>sl :set background=light<CR>
nmap <Leader>sd :set background=dark<CR>

set background=light
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
function! ToggleObsess()
    let root=FindRootDirectory()
    if !empty(root)
        let session=root."/.session.vim"
    else
        let cwd=getcwd()
        let session=cwd."/.session.vim"
    endif

    if !empty(glob(session))
        echo "Loading previous session..."
        source session
    else
        echo "Creating new session..."
        exe 'Obsess ' . session
    endif
endfunction

nmap <Leader>s :call ToggleObsess()<CR>
" }

" Vim-Rooter {
let g:rooter_change_directory_for_non_project_files = 'current'
let g:rooter_patterns                               = ['stack.yaml', '.git', '.git/', '.projectfile']
let g:rooter_silent_chdir                           = 1
" }

" Goyu {
nmap <Leader>yo :Goyo <bar> highlight StatusLineNC ctermfg=white<CR>
" }

" HTML {
let g:syntastic_html_tidy_ignore_errors = ["missing <!DOCTYPE> declaration",
            \ "plain text isn't allowed in <head> elements",
            \ "Info: <head> previously mentioned",
            \ "inserting implicit <body>",
            \ "inserting missing 'title' element",
            \ "trimming empty <i>"]
" }
