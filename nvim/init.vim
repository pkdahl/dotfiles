" Hybrid line numbers
"
set number
set relativenumber

" Indentation

set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set autoindent
set smartindent

set foldmethod=marker

" Dispaly hidden characters
" ':set list' to show hidden characters
set listchars=tab:→\ ,eol:¬

set scrolloff=5

" Plugins {{{

if !empty(trim(system('command -v fzf')))
let s:fzf = trim(system('command -v fzf'))
let g:fzf_opt = substitute(s:fzf, "\/bin\/", "\/opt\/", "")
unlet s:fzf
endif

call plug#begin('~/.local/share/nvim/plugged')

Plug 'airblade/vim-gitgutter'
Plug 'arcticicestudio/nord-vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/vim-easy-align'
Plug 'ledger/vim-ledger'
Plug 'nathangrigg/vim-beancount'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'vimwiki/vimwiki'

if exists("g:fzf_opt")
    Plug g:fzf_opt
    Plug 'junegunn/fzf.vim'
endif

call plug#end()
" }}}
" Mappings {{{

" Default Leader is \. want to use backspace as well
map <BS> <Leader>

inoremap <special> jk <Esc>
nnoremap <silent> <leader> s :set spell! <CR>
nnoremap <Leader> <Space> :nohlsearch <CR>

nnoremap <Leader>bn :bnext <CR>
nnoremap <Leader>bp :bprev <CR>

" n00b help

nnoremap <Left> :echoe "Use h" <CR>
nnoremap <Right> :echoe "Use l" <CR>
nnoremap <Up> :echoe "Use k" <CR>
nnoremap <Down> :echoe "Use j" <CR>

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" }}}

" Theme
augroup nord-overrides
    autocmd!
    " autocmd ColorScheme nord highlight Comment ctermfg=14
    autocmd ColorScheme nord highlight Folded ctermfg=Blue
augroup END
silent! colorscheme nord

let g:lightline = { 'colorscheme': 'nord' }

" Python
let g:python_host_prog = trim(system('command -v python2'))
let g:python3_host_prog = trim(system('command -v python3'))

" vimwiki

let g:vimwiki_list = [{ 'path': '~/Documents/notes/' },
                     \{ 'path': '~/Documents/houston/' }]

" vim: set foldmethod=marker foldlevel=0
