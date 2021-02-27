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
Plug 'chrisbra/csv.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/vim-easy-align'
Plug 'ledger/vim-ledger'
Plug 'liuchengxu/vim-which-key'
Plug 'nathangrigg/vim-beancount'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'vimwiki/vimwiki'
Plug 'voldikss/vim-floaterm'

if exists("g:fzf_opt")
    Plug g:fzf_opt
    Plug 'junegunn/fzf.vim'
endif

if has("nvim-0.5.0")
    Plug 'neovim/nvim-lsp'
    Plug 'lifepillar/vim-mucomplete'
endif

Plug 'sbdchd/neoformat'

call plug#end()
" }}}
" Mappings {{{

" Default Leader is \. want to use backspace as well
let g:mapleader = ' '
let g:maplocalleader = ','
map <bs> <leader>

call which_key#register(' ', 'g:which_key_map')
let g:which_key_map = {}
nnoremap <silent> <leader> :<c-u>WhichKey '<space>'<cr>
vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<space>'<cr>

inoremap <special> jk <esc>
" nnoremap <silent> <leader> s :set spell! <CR>
" nnoremap <Leader> <Space> :nohlsearch <CR>

let g:which_key_map.b = {
    \ 'name' : '+buffer',
    \ 'd' : ['bd'       , 'delete-buffer']  ,
    \ 'f' : ['bfirst'   , 'first-buffer']   ,
    \ 'n' : ['bnext'    , 'next-buffer']    ,
    \ 'p' : ['bprevious', 'previous-buffer'],
    \ 'B' : [':Buffers'  , 'fzf-buffer']     ,
    \ 'l' : [':BLines'   , 'fzf-blines']     ,
    \ 'L' : [':Lines'    , 'fzf-lines']      ,
    \ }

let g:which_key_map.f = {
    \ 'name' : '+files',
    \ 'f'    : [':Files'   , 'files'] ,
    \ 'g'    : [':GFiles'  , 'git files'],
    \ 'G'    : [':GFiles?' , 'modified git files'] ,
    \ }

let g:which_key_map.s = {
    \ 'name' : '+search',
    \ 'r'    : ['Rg' , 'fzf-ripgrep'],
    \ }

let g:which_key_map.w = {
    \ 'name' : '+windows',
    \ 'd' : ['<C-w>c' , 'delete-window']      ,
    \ 'v' : ['<C-w>v' , 'split-window-right'] ,
    \ '=' : ['<C-w>=' , 'balance-window']     ,
    \ }

let g:which_key_map.z = {
    \ 'name' : '+fold',
    \ 'c'    : ['zc', 'close-fold']        ,
    \ 'o'    : ['zo', 'open-fold']         ,
    \ 'j'    : ['zj', 'start-next']        ,
    \ 'k'    : ['zk', 'end-previous']      ,
    \ 'R'    : ['zR', 'open-all-folds']    ,
    \ '<'    : ['[z', 'move-start-current'],
    \ '>'    : [']z', 'move-end-current']  ,
    \ }

" n00b help

nnoremap <Left> :echoe "Use h" <CR>
nnoremap <Right> :echoe "Use l" <CR>
nnoremap <Up> :echoe "Use k" <CR>
nnoremap <Down> :echoe "Use j" <CR>

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" }}}

" Completion

set completeopt+=menuone
let g:mucomplete#enable_auto_at_startup = 1
let g:mucomplete#completion_delay = 1

" Formatting

let g:neoformat_enabled_ocaml = ['ocamlformat']

" LSP

if has("nvim-0.5.0")
:lua << END
    require'lspconfig'.ocamllsp.setup{}
END
endif

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
