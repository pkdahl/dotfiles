" hybrid line numbers
set number
set relativenumber

" indentation

set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set autoindent
set smartindent

set foldmethod=marker

" plugins

call plug#begin('~/.local/share/nvim/plugged')

" This doesn't seem to work if variable is local to the script
" let g:fzf_path = trim(system('brew --prefix')) . '/opt/fzf'
" if !empty(glob(g:fzf_path))
"     Plug g:fzf_path
"     Plug 'junegunn/fzf.vim'
" endif

Plug 'airblade/vim-gitgutter'
Plug 'arcticicestudio/nord-vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/vim-easy-align'
" Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tpope/vim-fugitive'
Plug 'vimwiki/vimwiki'

call plug#end()

"Mappings

let mapleader=","
nnoremap , <Nop>

inoremap <special> jk <esc>
nnoremap <silent> <leader> s :set spell! <cr>
nnoremap <leader> <space> :nohlsearch <cr>

nnoremap <Leader>bn :bnext <CR>
nnoremap <Leader>bp :bprev <CR>

" dispaly hidden characters
" ':set list' to show hidden characters
set listchars=tab:→\ ,eol:¬

" n00b help

nnoremap <Left> :echoe "Use h" <cr>
nnoremap <Right> :echoe "Use l" <cr>
nnoremap <Up> :echoe "Use k" <cr>
nnoremap <Down> :echoe "Use j" <cr>

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" theme

silent! colorscheme nord
let g:lightline = { 'colorscheme': 'nord' }

" Python
let g:python_host_prog = trim(system('command -v python2'))
let g:python3_host_prog = trim(system('command -v python3'))

" vimwiki

let g:vimwiki_list = [{ 'path': '~/Documents/notes/' },
                     \{ 'path': '~/Documents/houston/' }]
