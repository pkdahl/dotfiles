#!/usr/bin/env bash

. ../helpers.sh

VIM_CACHE_HOME=$HOME/.cache/vim
DOT_VIM_DIR=$(pwd)

if [[ ! -d $VIM_CACHE_HOME ]]; then
	echo_info "Creating $VIM_CACHE_HOME"
	mkdir -p $VIM_CACHE_HOME
fi

if [[ ! -L $HOME/.vim ]]; then
	echo_info "Symlinking .vim"
	ln -sf $DOT_VIM_DIR $HOME/.vim
fi

# Install packs

VIM_PACK_DIR=$DOT_VIM_DIR/pack

function install_pack() {
	if [ ! -d $VIM_PACK_DIR/dist/start/$2 ]; then
		echo_info "Installing $1"
		mkdir -p $VIM_PACK_DIR/dist/start
		git clone $1 $VIM_PACK_DIR/dist/start/$2
		vim -u NONE -c "helptags $VIM_PACK_DIR/dist/start/$2/doc" -c q
	fi
}

# if [ ! -d $DOT_VIM_DIR/pack/themes/opt/solarized8 ]; then
#	echo_info "Installing solarized8"
#	mkdir -p $DOT_VIM_DIR/pack/themes/opt
#	git clone https://github.com/lifepillar/vim-solarized8.git $DOT_VIM_DIR/pack/themes/opt/solarized8
# fi

install_pack https://github.com/ctrlpvim/ctrlp.vim.git ctrlp
install_pack https://github.com/tpope/vim-commentary.git commentary
install_pack https://github.com/tpope/vim-fugitive fugitive
install_pack https://github.com/masukomi/vim-markdown-folding.git markdown-folding
install_pack https://github.com/vimwiki/vimwiki.git vimwiki
install_pack https://github.com/tmux-plugins/vim-tmux.git tmux
install_pack https://github.com/tmux-plugins/vim-tmux-focus-events.git tmux-focus-events
install_pack https://github.com/itchyny/lightline.vim.git lightline
install_pack https://github.com/edkolev/tmuxline.vim.git tmuxline

echo_done "Setup vim"
