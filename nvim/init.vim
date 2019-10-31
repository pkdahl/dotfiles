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

" plugins

if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
	silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
		\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.local/share/nvim/plugged')

" This doesn't seem to work if variable is local to the script
let g:fzf_path = trim(system('brew --prefix')) . '/opt/fzf'
if !empty(glob(g:fzf_path))
    Plug g:fzf_path
    Plug 'junegunn/fzf.vim'
endif

Plug 'arcticicestudio/nord-vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/vim-easy-align'
Plug 'ledger/vim-ledger'
Plug 'vimwiki/vimwiki'

call plug#end()

" keybindings

let mapleader=","

inoremap <special> jk <esc>
nnoremap <silent> <leader>s :set spell!<CR>

" n00b help

nnoremap <Left> :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up> :echoe "Use k"<CR>
nnoremap <Down> :echoe "Use j"<CR>

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" theme

colorscheme nord
let g:lightline = { 'colorscheme': 'nord' }

" vimwiki

let g:vimwiki_list = [{'path': '~/Documents/notes/'},
                     \{'path': '~/Documents/houston/'}]
