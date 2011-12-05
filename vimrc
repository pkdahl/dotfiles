set nocompatible
set ruler
set tabstop=4
set shiftwidth=4
set shiftround
set autoindent
set nobackup " Do not make backup files
set textwidth=79
set foldmethod=marker

"Allow backspacing in insert mode
set bs=2

if &t_Co > 2 || has("gui_running")
    syntax on
endif
